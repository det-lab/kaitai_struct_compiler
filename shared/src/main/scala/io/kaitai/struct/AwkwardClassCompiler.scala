//package io.kaitai.struct.languages
package io.kaitai.struct

import io.kaitai.struct._

import io.kaitai.struct.CppRuntimeConfig._

import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType._

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr

import io.kaitai.struct.format._

import io.kaitai.struct.languages.components._
//import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic}

import io.kaitai.struct.precompile.CalculateSeqSizes

//import io.kaitai.struct.translators._
import io.kaitai.struct.translators.{CppTranslator, TypeDetector}
//import io.kaitai.struct.translators.RubyTranslator

import scala.collection.mutable.ListBuffer


class AwkwardClassCompiler(classSpecs: ClassSpecs, topClass: ClassSpec, config: RuntimeConfig) extends AbstractCompiler {
  import AwkwardClassCompiler._

  val importListSrc = new CppImportList
  val importListHdr = new CppImportList

  val provider = new ClassTypeProvider(classSpecs, topClass)
  val translator = new CppTranslator(provider, importListSrc, importListHdr, config)
  val links = ListBuffer[(String, String, String)]()

  def nowClass: ClassSpec = provider.nowClass
  def nowClassName = provider.nowClass.name
  var currentTable: String = ""

  val outSrc = new StringLanguageOutputWriter(indent)
  val outHdr = new StringLanguageOutputWriter(indent)
  def outFileNameSource(topClassName: String): String = s"$topClassName.cpp"
  def outFileNameHeader(topClassName: String): String = s"$topClassName.h"
  
  override def compile: CompileLog.SpecSuccess = {
    
    // Header file - Pre-Processor directives
    outHdr.puts("#include \"kaitai/kaitaistruct.h\"")
    outHdr.puts("#include \"awkward/builder/ArrayBuilderOptions.h\"")
    outHdr.puts
    outHdr.puts("namespace ak = awkward;")
    outHdr.puts

    // Header file - Contents
    outHdr.puts("ak::ArrayBuilder _read(kaitai::kstream* ks);")

    // Source file - Pre-Processor directives
    outSrc.puts("#include <iostream>")
    outSrc.puts("#include <fstream>")
    outSrc.puts("#include <string>")
    outSrc.puts
    outSrc.puts("#include \"kaitai/kaitaistruct.h\"")
    outSrc.puts
    outSrc.puts("#include \"awkward/Slice.h\"")
    outSrc.puts("#include \"awkward/builder/ArrayBuilder.h\"")
    outSrc.puts("#include \"awkward/builder/ArrayBuilderOptions.h\"")
    outSrc.puts
    outSrc.puts("using namespace std;")
    outSrc.puts("namespace ak = awkward;")
    outSrc.puts
    
    // Source file - Contents
    outSrc.puts(s"// Data type: ${type2class(nowClassName)}")
    outSrc.puts(s"ak::ArrayBuilder _read(kaitai::kstream* ks) {")
    outSrc.inc
    outSrc.puts
    outSrc.puts(s"// initialize array for ${type2class(nowClassName)} seq")
    outSrc.puts(s"ak::ArrayBuilder ${type2class(nowClassName)}s(ak::ArrayBuilderOptions(1024, 2.0));")
    outSrc.puts
    outSrc.puts(s"while(!ks->is_eof()) {")
    outSrc.inc
    outSrc.puts(s"${type2class(nowClassName)}s.beginrecord();")

    compileClass(topClass)

    outSrc.puts(s"${type2class(nowClassName)}s.endrecord();")

    outSrc.dec
    outSrc.puts("}")

    // Send outSrc and outHdr to be written to files
    CompileLog.SpecSuccess(
      "",//type2class(topClassName.head),
      results(topClass).map { case (fileName, contents) => CompileLog.FileSuccess(fileName, contents) }.toList
      )
  }

  // set filename to text contained in outSrc and outHdr
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

    //curClass.enums.foreach { case(enumName, enumColl) => compileEnum(enumName, enumColl) }

    // Recursive types
    curClass.types.foreach { case (typeName, intClass) => compileClass(intClass) }

    outSrc.dec
    outSrc.puts("}")
  }

  def indent: String = "\t"

  def compileSeq(className: List[String], curClass: ClassSpec): Unit = {
    //tableStart(className, "seq")
    val currSeq: String = s"${type2class(className)}"
    
    

    CalculateSeqSizes.forEachSeqAttr(curClass, (attr, seqPos, _, _) => {
      attr.id match {
        case NamedIdentifier(name) =>
          readSeq(className, seqPosToStr(seqPos), attr, name)
        case NumberedIdentifier(n) =>
          readSeq(className, seqPosToStr(seqPos), attr, s"_${NumberedIdentifier.TEMPLATE}$n")
      }
    })
  }

  def readSeq(curClass: List[String], pos: Option[String], attr: AttrLikeSpec, name: String): Unit = {
    val dataType = attr.dataType
    val dataTypeStr = dataTypeName(dataType) /*dataType match {
      case st: SwitchType =>
        compileSwitch(name, st)
        s"switch (${expressionType(st.on, name)})"
      case _ =>
        dataTypeName(dataType)*/
    //}
    val topClassName = topClass.nameAsStr
 
    // TODO
    dataType match {
      case ut: UserType =>
        outSrc.puts(s"// Read commands for sequence: ${type2class(ut.name)}")
        outSrc.puts
      case number =>
        outSrc.puts(s"// currently on "+name)
        outSrc.puts(s"${kaitaiType2NativeType(dataType)} ${name} = ks->read_${dataTypeStr}();")
        outSrc.puts(s"${topClassName}s.field_check($name);")
        outSrc.puts(s"${topClassName}s.integer($name)")
        outSrc.puts
      case string =>
        outSrc.puts(s"// currently on "+name)
        outSrc.puts(s"${kaitaiType2NativeType(dataType)} ${name} = ks->read_${dataTypeStr}();")
        outSrc.puts(s"${topClassName}s.field_check($name);")
        outSrc.puts(s"${topClassName}s.string($name)")
        outSrc.puts
    }

     // Add user type links
    dataType match {
      case ut: UserType =>
        outSrc.puts(s"// Read commands for sequence: ${type2class(ut.name)}")
        outSrc.puts
      case _ =>
        outSrc.puts(s"// currently on "+name)
        outSrc.puts(s"${kaitaiType2NativeType(dataType)} ${name} = ks->read_${dataTypeStr}();")
        outSrc.puts(s"${topClassName}s.field_check($name);")
        outSrc.puts
    }
  }  

  def expression(e: Ast.expr): String = translator.translate(e) 

  def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io->read_${t.apiCall(defEndian)}()"
      case blt: BytesLimitType =>
        s"$io->read_bytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io->read_bytes_full()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io->read_bytes_term($terminator, $include, $consume, $eosError)"
      case BitsType1 =>
        s"$io->read_bits_int(1)"
      case BitsType(width: Int) =>
        s"$io->read_bits_int($width)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(USER_TYPE_NO_PARENT) => "// This user type has no parent"
            case Some(fp) => translator.translate(fp)
            case None =>
              config.cppConfig.pointers match {
                case RawPointers | UniqueAndRawPointers => "this"
                case SharedPointers => s"shared_from_this()"
              }
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", m__is_le"
            case _ => ""
          }
          "//, $parent, ${privateMemberName(RootIdentifier)}$addEndian"
        }
        config.cppConfig.pointers match {
          case RawPointers =>
            s"new ${types2class(t.name)}($addParams$io$addArgs)"
          case SharedPointers =>
            s"std::make_shared<${types2class(t.name)}>($addParams$io$addArgs)"
          case UniqueAndRawPointers =>
            importListSrc.addSystem("memory")
            // C++14
            //s"std::make_unique<${types2class(t.name)}>($addParams$io$addArgs)"
            s"std::unique_ptr<${types2class(t.name)}>(new ${types2class(t.name)}($addParams$io$addArgs))"
        }
    }
  }

  /*def compileSwitch(attrName: String, st: SwitchType): Unit = {

    links += ((s"$currentTable:${attrName}_type", s"${currentTable}_${attrName}_switch", ""))
    
    var lineNum = 0
    st.cases.foreach { case (caseExpr, caseType) =>
      caseType match {
        case ut: UserType =>
          val exprStr = htmlEscape(translator.translate(caseExpr))
          val portName = s"case$lineNum"
          lineNum += 1
          links += ((s"${currentTable}_${attrName}_switch:$portName", type2class(ut.name) + "__seq", ""))
        case _ =>
          // ignore, no links
      }
    }
  }*/
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
