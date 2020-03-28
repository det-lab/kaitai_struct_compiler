// From languages/CppCompiler.scala
//package io.kaitai.struct.languages
package io.kaitai.struct

import io.kaitai.struct.CppRuntimeConfig._
import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{CppTranslator, TypeDetector}

// From GraphvizClassCompiler.scala
import io.kaitai.struct.datatype.DataType
//import io.kaitai.struct.datatype.DataType._
//import io.kaitai.struct.exprlang.Ast
//import io.kaitai.struct.format._
import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}
import io.kaitai.struct.precompile.CalculateSeqSizes
import io.kaitai.struct.translators.RubyTranslator
import scala.collection.mutable.ListBuffer


class AwkwardClassCompiler(classSpecs: ClassSpecs, topClass: ClassSpec) extends AbstractCompiler {
  import AwkwardClassCompiler._

  val outSrc = new StringLanguageOutputWriter(indent)
  val outHdr = new StringLanguageOutputWriter(indent)

  val provider = new ClassTypeProvider(classSpecs, topClass)
  val translator = new RubyTranslator(provider)
  val links = ListBuffer[(String, String, String)]()
  val extraClusterLines = new StringLanguageOutputWriter(indent)

  def nowClass: ClassSpec = provider.nowClass
  def nowClassName = provider.nowClass.name
  var currentTable: String = ""

  def outFileNameSource(topClassName: String): String = s"$topClassName.cpp"
  def outFileNameHeader(topClassName: String): String = s"$topClassName.h"
  
  override def compile: CompileLog.SpecSuccess = {
    
    // Header file
    outHdr.puts("#include \"kaitai/kaitaistruct.h\"")
    outHdr.puts("#include \"awkward/builder/ArrayBuilderOptions.h\"")
    outHdr.puts
    outHdr.puts("namespace ak = awkward;")
    outHdr.puts
    outHdr.puts("ak::ArrayBuilder _read(kaitai::kstream* ks);")

    // Source file Pre-Processor directives
    outSrc.puts("#include \"kaitai/kaitaistruct.h\"")
    outSrc.puts("#include <stdint.h>")
    outSrc.puts("#include \"awkward/Slice.h\"")
    outSrc.puts("#include \"awkward/builder/ArrayBuilder.h\"")
    outSrc.puts("#include \"awkward/builder/ArrayBuilderOptions.h\"")
    outSrc.puts
    outSrc.puts("namespace ak = awkward;")
    outSrc.puts

    compileClass(topClass)

    outSrc.dec
    outSrc.puts("}")

    CompileLog.SpecSuccess(
      "",//type2class(topClassName.head),
      results(topClass).map { case (fileName, contents) => CompileLog.FileSuccess(fileName, contents) }.toList
      )

  }

  def results(topClass: ClassSpec): Map[String, String] = { 
    val className = topClass.nameAsStr
    Map(
      outFileNameSource(className) -> (outSrc.result),
      outFileNameHeader(className) -> (outHdr.result)
    )
    }

  def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass
    val className = curClass.name
    // ${type2class(className)} here returns "animal"
    // type2display(the above) returns "Animal"

    // Sequence
    compileSeq(className, curClass)

    curClass.instances.foreach { case (instName, instSpec) =>
      instSpec match {
        case pis: ParseInstanceSpec =>
          tableStart(className, s"inst__${instName.name}")
          compileParseInstance(className, instName, pis)
          tableEnd
        case vis: ValueInstanceSpec =>
          outSrc.puts(s"bingo bango")
          tableValueInstance(className, instName.name, vis)
      }
    }

//    curClass.enums.foreach { case(enumName, enumColl) => compileEnum(enumName, enumColl) }

    outSrc.add(extraClusterLines)
    extraClusterLines.clear()

    // Recursive types
    curClass.types.foreach { case (typeName, intClass) => compileClass(intClass) }

    outSrc.dec
    outSrc.puts("}")
  }

  def compileSeq(className: List[String], curClass: ClassSpec): Unit = {
    tableStart(className, "seq")
    
    outSrc.puts(s"animalbeginrecord();")
    outSrc.puts

    CalculateSeqSizes.forEachSeqAttr(curClass, (attr, seqPos, _, _) => {
      attr.id match {
        case NamedIdentifier(name) =>
          tableRow(className, seqPosToStr(seqPos), attr, name)
        case NumberedIdentifier(n) =>
          tableRow(className, seqPosToStr(seqPos), attr, s"_${NumberedIdentifier.TEMPLATE}$n")
      }
    })

    tableEnd
  }

  def compileParseInstance(className: List[String], id: InstanceIdentifier, inst: ParseInstanceSpec): Unit = {
    val name = id.name
    val lastInstPos = inst.pos
    lastInstPos match {
      case Some(pos) =>
        val posStr = expressionPos(pos, name)
        tableRow(className, Some(posStr), inst, name)

      case None =>
        tableRow(className, None, inst, name)
    }
  }

  val HEADER_BGCOLOR = "#E0FFE0"
  val SWITCH_HEADER_BGCOLOR = "#F0F2E4"
  val TH_START = "<TD BGCOLOR=\"" + HEADER_BGCOLOR + "\">"
  val SWITCH_TH_START = "<TD BGCOLOR=\"" + SWITCH_HEADER_BGCOLOR + "\">"

  def tableStart(className: List[String], extra: String): Unit = {
    currentTable = s"${type2class(className)}__$extra"
    outSrc.puts(s"// current table: $currentTable")
    outSrc.inc
  }

  def tableEnd: Unit = {
    outSrc.dec
    outSrc.puts("animal.endrecord();")
  }

  val STYLE_EDGE_TYPE = "style=bold"
  val STYLE_EDGE_MISC = "color=\"#404040\""
  val STYLE_EDGE_POS = STYLE_EDGE_MISC
  val STYLE_EDGE_SIZE = STYLE_EDGE_MISC
  val STYLE_EDGE_REPEAT = STYLE_EDGE_MISC
  val STYLE_EDGE_VALUE = STYLE_EDGE_MISC

  // "constructRead"
  def tableRow(curClass: List[String], pos: Option[String], attr: AttrLikeSpec, name: String): Unit = {
    val dataType = attr.dataType
    val sizeStr = dataTypeSizeAsString(dataType, name)

    val dataTypeStr = dataType match {
      case st: SwitchType =>
        compileSwitch(name, st)
        s"switch (${expressionType(st.on, name)})"
      case _ =>
        dataTypeName(dataType)
    }

    outSrc.puts(s"${kaitaiType2NativeType(dataType)} data = m__io.read_$dataTypeStr();")
    outSrc.puts(s"animal.field_check("+name+");")
    outSrc.puts(s"animal.string(data);")
    outSrc.puts(s"pos.getOrElse = "+pos.getOrElse("..."))
    outSrc.puts(s"sizeStr = " + sizeStr)
    outSrc.puts(s"dataTypeStr = $dataTypeStr")

    // Add user type links
    dataType match {
      case ut: UserType =>
        links += ((s"$currentTable:${name}_type", type2class(ut.name) + "__seq", STYLE_EDGE_TYPE))
      case _ =>
        // ignore, no links
    }

    val portName = name + "__repeat"
    attr.cond.repeat match {
      case RepeatExpr(ex) =>
        outSrc.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">repeat " +
          expression(ex, s"$currentTable:$portName", STYLE_EDGE_REPEAT) +
          " times</TD></TR>")
      case RepeatUntil(ex) =>
        provider._currentIteratorType = Some(dataType)
        outSrc.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">repeat until " +
          expression(ex, s"$currentTable:$portName", STYLE_EDGE_REPEAT) +
          "</TD></TR>")
      case RepeatEos =>
        outSrc.puts("<TR><TD COLSPAN=\"4\" PORT=\"" + portName + "\">repeat to end of stream</TD></TR>")
      case NoRepeat =>
      // no additional line
    }
  }

  def tableValueInstance(curClass: List[String], name: String, inst: ValueInstanceSpec): Unit = {
    currentTable = s"${type2class(curClass)}__inst__$name"

    outSrc.puts(s"$currentTable" + " [label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">")
    outSrc.inc
    outSrc.puts(s"<TR>${TH_START}id</TD>${TH_START}value</TD></TR>")

    outSrc.puts(
      s"<TR><TD>$name</TD>" +
      "<TD>" +
      expression(inst.value, currentTable, STYLE_EDGE_VALUE) +
      "</TD></TR>"
    )

    tableEnd
  }

  def compileSwitch(attrName: String, st: SwitchType): Unit = {

    links += ((s"$currentTable:${attrName}_type", s"${currentTable}_${attrName}_switch", STYLE_EDGE_TYPE))
    extraClusterLines.puts(s"${currentTable}_${attrName}_switch " + "[label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">")
    extraClusterLines.inc
    extraClusterLines.puts(s"<TR>${SWITCH_TH_START}case</TD>${SWITCH_TH_START}type</TD></TR>")

    var lineNum = 0
    st.cases.foreach { case (caseExpr, caseType) =>
      caseType match {
        case ut: UserType =>
          val exprStr = htmlEscape(translator.translate(caseExpr))
          val portName = s"case$lineNum"
          lineNum += 1
          extraClusterLines.puts(
            "<TR><TD>" + exprStr + "</TD>" + "" +
              "<TD PORT=\"" + portName + "\">" + type2display(ut.name) + "</TD></TR>"
          )
          links += ((s"${currentTable}_${attrName}_switch:$portName", type2class(ut.name) + "__seq", STYLE_EDGE_TYPE))
        case _ =>
          // ignore, no links
      }
    }

    extraClusterLines.dec
    extraClusterLines.puts("</TABLE>>];")
  }

  val END_OF_STREAM = "⇲"
  val UNKNOWN = "..."

  /**
    * Determine visual interpretation of data type's size to be used in
    * a displayed table.
    *
    * @param dataType data type to analyze
    * @param attrName attribute name to be used to generate port name for affected vars linking
    * @return a String that should be displayed in table's column "size"
    */
  def dataTypeSizeAsString(dataType: DataType, attrName: String): String = {
    dataType match {
      case _: BytesEosType => END_OF_STREAM
      case blt: BytesLimitType => expressionSize(blt.size, attrName)
      case StrFromBytesType(basedOn, _) => dataTypeSizeAsString(basedOn, attrName)
      case utb: UserTypeFromBytes => dataTypeSizeAsString(utb.bytes, attrName)
      case EnumType(_, basedOn) => dataTypeSizeAsString(basedOn, attrName)
      case _ =>
        CalculateSeqSizes.dataTypeBitsSize(dataType) match {
          case FixedSized(n) =>
            if (n % 8 == 0) {
              s"${n / 8}"
            } else {
              s"${n}b"
            }
          case DynamicSized => UNKNOWN
          case NotCalculatedSized | StartedCalculationSized =>
            throw new RuntimeException("Should never happen: problems with CalculateSeqSizes")
        }
    }
  }

  def expressionSize(ex: Ast.expr, attrName: String): String = {
    expression(ex, getGraphvizNode(nowClassName, nowClass, attrName) + s":${attrName}_size", STYLE_EDGE_SIZE)
  }

  def expressionPos(ex: Ast.expr, attrName: String): String = {
    expression(ex, getGraphvizNode(nowClassName, nowClass, attrName) + s":${attrName}_pos", STYLE_EDGE_POS)
  }

  def expressionType(ex: Ast.expr, attrName: String): String = {
    expression(ex, getGraphvizNode(nowClassName, nowClass, attrName) + s":${attrName}_type", STYLE_EDGE_VALUE)
  }

  def expression(e: Ast.expr, portName: String, style: String): String = {
    affectedVars(e).foreach((v) =>
      links += ((v, portName, style))
    )
    htmlEscape(translator.translate(e))
  }

  def affectedVars(e: Ast.expr): List[String] = {
    e match {
      case Ast.expr.BoolOp(op, values) =>
        values.flatMap(affectedVars).toList
      case Ast.expr.BinOp(left, op, right) =>
        affectedVars(left) ++ affectedVars(right)
      case Ast.expr.UnaryOp(op, operand) =>
        affectedVars(operand)
      case Ast.expr.IfExp(condition, ifTrue, ifFalse) =>
        affectedVars(condition) ++ affectedVars(ifTrue) ++ affectedVars(ifFalse)
      //      case expr.Dict(keys, values) =>
      case Ast.expr.Compare(left, ops, right) =>
        affectedVars(left) ++ affectedVars(right)
      //      case expr.Call(func, args) =>
      case Ast.expr.IntNum(_) | Ast.expr.FloatNum(_) | Ast.expr.Str(_) | Ast.expr.Bool(_) =>
        List()
      case _: Ast.expr.EnumByLabel =>
        List()
      case Ast.expr.EnumById(_, id, _) =>
        affectedVars(id)
      case Ast.expr.Attribute(value, attr) =>
        val targetClass = translator.detectType(value)
        targetClass match {
          case t: UserType =>
            // Although, technically, in a clause like "foo.bar" both "foo" and
            // "bar" seem to be affecting the result, graphs seems to be more
            // readable if we'll only use "bar" for referencing, without doing
            // distinct link to all intermediate path walking nodes.

            // Uncomment this one to get "affected" references to all
            // intermediate nodes
            //affectedVars(value) ++ List(resolveTypedNode(t, attr.name))

            List(resolveTypedNode(t, attr.name))
          case _ =>
            affectedVars(value)
        }
      case Ast.expr.Call(func, args) =>
        val fromFunc = func match {
          case Ast.expr.Attribute(obj: Ast.expr, methodName: Ast.identifier) => affectedVars(obj)
        }
        fromFunc ::: affectedVars(Ast.expr.List(args))
      case Ast.expr.Subscript(value, idx) =>
        affectedVars(value) ++ affectedVars(idx)
      case SwitchType.ELSE_CONST =>
        // "_" is a special const for
        List()
      case Ast.expr.Name(id) =>
        if (id.name.charAt(0) == '_') {
          // other special consts like "_io", "_index", etc
          List()
        } else {
          // this must be local name, resolve it
          List(resolveLocalNode(id.name))
        }
      case Ast.expr.List(elts) =>
        elts.flatMap(affectedVars).toList
    }
  }

//  def resolveNode(s: String): String = {
//    s"$currentTable:${s}_type"
//  }

  /**
    * Given a local name that should be pertinent to current class, resolve it into
    * full-blown Graphviz port reference (i.e. given something `foo` should yield
    * `stuff__seq:foo_type` is `foo` is a sequence element)
    *
    * @param s name to resolve
    * @return
    */
  def resolveLocalNode(s: String): String =
    resolveNodeForClass(nowClassName, nowClass, s)

  def resolveTypedNode(t: UserType, s: String): String = {
    val className = t.name
    val classSpec = t.classSpec.get
    resolveNodeForClass(className, classSpec, s)
  }

  def resolveNodeForClass(className: List[String], cs: ClassSpec, s: String): String =
    s"${getGraphvizNode(className, cs, s)}:${s}_type"

  def getGraphvizNode(className: List[String], cs: ClassSpec, s: String): String = {
    cs.seq.foreach { (attr) =>
      val name = attr.id match {
        case NamedIdentifier(attrName) =>
          attrName
        case NumberedIdentifier(n) =>
          s"_${NumberedIdentifier.TEMPLATE}$n"
      }
      if (name == s) {
        return s"${type2class(className)}__seq"
      }
    }

    cs.params.foreach { (attr) =>
      val name = attr.id match {
        case NamedIdentifier(attrName) =>
          attrName
        case NumberedIdentifier(n) =>
          s"_${NumberedIdentifier.TEMPLATE}$n"
      }
      if (name == s) {
        return s"${type2class(className)}__params"
      }
    }

    cs.instances.get(InstanceIdentifier(s)).foreach((inst) =>
      return s"${type2class(className)}__inst__$s"
    )

    throw new RuntimeException(s"unable to resolve node '$s' in type '${type2display(className)}'")
  }

  def indent: String = "\t"

}

// extension of LanguageCompilerStatic
object AwkwardClassCompiler extends LanguageCompilerStatic {
  // FIXME: Unused, should be probably separated from LanguageCompilerStatic
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = ???

  def type2class(name: List[String]) = name.last
  def type2display(name: List[String]) = name.map(Utils.upperCamelCase).mkString("::")

  def kstructName = "kaitai::kstruct"
  def kstreamName = "kaitai::kstream"

  //def kaitaiType2NativeType(config: something = CppRuntimeConfig, attrType: DataType, absolute: Boolean = false): String = {
  def kaitaiType2NativeType(attrType: DataType, absolute: Boolean = false): String = {
    attrType match {
      case Int1Type(false) => "uint8_t"
      case IntMultiType(false, Width2, _) => "uint16_t"
      case IntMultiType(false, Width4, _) => "uint32_t"
      case IntMultiType(false, Width8, _) => "uint64_t"

      case Int1Type(true) => "int8_t"
      case IntMultiType(true, Width2, _) => "int16_t"
      case IntMultiType(true, Width4, _) => "int32_t"
      case IntMultiType(true, Width8, _) => "int64_t"

      case FloatMultiType(Width4, _) => "float"
      case FloatMultiType(Width8, _) => "double"

      case BitsType(_) => "uint64_t"

      case _: BooleanType => "bool"
      case CalcIntType => "int32_t"
      case CalcFloatType => "double"

      case _: StrType => "std::string"
      case _: BytesType => "std::string"

      case t: UserType =>
        val typeStr = types2class(if (absolute) {
          t.classSpec.get.name
        } else {
          t.name
        }) 
        s"$typeStr*"
        //config.pointers match {
        //  case RawPointers => s"$typeStr*"
        //  case SharedPointers => s"std::shared_ptr<$typeStr>"
        //  case UniqueAndRawPointers =>
        //    if (t.isOwning) s"std::unique_ptr<$typeStr>" else s"$typeStr*"
        //}

      case t: EnumType =>
        types2class(if (absolute) {
          t.enumSpec.get.name
        } else {
          t.name
        })

      case ArrayTypeInStream(inType) => s"std::vector<${kaitaiType2NativeType(inType, absolute)}>*"
      case CalcArrayType(inType) => s"std::vector<${kaitaiType2NativeType(inType, absolute)}>*"
      case KaitaiStreamType => s"$kstreamName*"
      case KaitaiStructType => s"$kstructName*"
      case CalcKaitaiStructType => s"$kstructName*"

      case st: SwitchType =>
        kaitaiType2NativeType(combineSwitchType(st), absolute)
    }
  }

  def types2class(typeName: Ast.typeId) = {
    typeName.names.map(type2class).mkString(
      if (typeName.absolute) "::" else "",
      "::",
      ""
    )
  }

  def types2class(components: List[String]) =
    components.map(type2class).mkString("::")

  def type2class(name: String) = name + "_t"

  def combineSwitchType(st: SwitchType): DataType = {
    val ct1 = TypeDetector.combineTypes(
      st.cases.filterNot {
        case (caseExpr, _: BytesType) => caseExpr == SwitchType.ELSE_CONST
        case _ => false
      }.values
    )
    if (st.isOwning) {
      ct1
    } else {
      ct1.asNonOwning
    }
  }
  
  def dataTypeName(dataType: DataType): String = {
    dataType match {
      case rt: ReadableType => rt.apiCall(None) // FIXME
      case ut: UserType => type2display(ut.name)
      //case FixedBytesType(contents, _) => contents.map(_.formatted("%02X")).mkString(" ")
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        val args = ListBuffer[String]()
        if (terminator != 0)
          args += s"term=$terminator"
        if (include)
          args += "include"
        if (!consume)
          args += "don't consume"
        if (!eosError)
          args += "ignore EOS"
        args.mkString(", ")
      case _: BytesType => ""
      case StrFromBytesType(basedOn, encoding) =>
        val bytesStr = dataTypeName(basedOn)
        val comma = if (bytesStr.isEmpty) "" else ", "
        s"str($bytesStr$comma$encoding)"
      case EnumType(name, basedOn) =>
        s"${dataTypeName(basedOn)}→${type2display(name)}"
      case BitsType(width) => s"b$width"
      case _ => dataType.toString
    }
  }

  def htmlEscape(s: String): String = {
    s.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\"", "&quot;")
  }

  /*
    * Converts bit-level position into byte/bit human-readable combination.
    * @param seqPos optional number of bits
    * @return fractional human-readable string which displays "bytes:bits",
    *         akin to "minutes:seconds" time display
    */
  def seqPosToStr(seqPos: Option[Int]): Option[String] = {
    seqPos.map { (pos) =>
      val posByte = pos / 8
      val posBit = pos % 8
      if (posBit == 0) {
        s"$posByte"
      } else {
        s"$posByte:$posBit"
      }
    }
  }
}
