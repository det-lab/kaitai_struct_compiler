package io.kaitai.struct

import io.kaitai.struct._
import io.kaitai.struct.CppRuntimeConfig._
import io.kaitai.struct.datatype._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.AwkwardCompiler
import io.kaitai.struct.languages.components._
//import io.kaitai.struct.languages.components.{LanguageCompiler, LanguageCompilerStatic, CppImportList}
import io.kaitai.struct.precompile.CalculateSeqSizes
import io.kaitai.struct.translators.{CppTranslator, TypeDetector}

import scala.collection.mutable.ListBuffer

class AwkwardClassCompiler(classSpecs: ClassSpecs, override val topClass: ClassSpec, config: RuntimeConfig) 
	extends ClassCompiler(classSpecs, topClass, config, AwkwardCompiler) 
{

  val importListSrc = new CppImportList
  val importListHdr = new CppImportList

  override val provider = new ClassTypeProvider(classSpecs, topClass)
  val translator = new CppTranslator(provider, importListSrc, importListHdr, config)
  val links = ListBuffer[(String, String, String)]()

  def nowClass: ClassSpec = provider.nowClass
  def nowClassName = provider.nowClass.name
  var currentTable: String = ""

  val outSrc = new StringLanguageOutputWriter(indent)
  val outHdr = new StringLanguageOutputWriter(indent)
  def outFileNameSource(topClassName: String): String = s"$topClassName.cpp"
  def outFileNameHeader(topClassName: String): String = s"$topClassName.h"
  
  override val topClassName = topClass.name//AsStr // "animal"

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
    outSrc.puts(s"// Data type: ${AwkwardCompiler.type2class(nowClassName)}")
    outSrc.puts(s"ak::ArrayBuilder _read(kaitai::kstream* ks) {")
    outSrc.inc
    outSrc.puts
    outSrc.puts(s"// initialize array for ${AwkwardCompiler.type2class(nowClassName)} seq")
    outSrc.puts(s"ak::ArrayBuilder ${AwkwardCompiler.type2class(nowClassName)}s(ak::ArrayBuilderOptions(1024, 2.0));")
    outSrc.puts
    outSrc.puts(s"while(!ks->is_eof()) {")
    outSrc.inc

    compileArray(topClass)

    outSrc.dec
    outSrc.puts("}")

    // Send outSrc and outHdr to be written to files
    CompileLog.SpecSuccess(
      "",//AwkwardCompiler.type2class(topClassName.head),
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

  def compileArray(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass
    val className = curClass.name
    // ${AwkwardCompilertype2class(className)} here returns "animal"
    // type2display(the above) returns "Animal"
    outSrc.puts(s"${topClassName}s.beginrecord();")
    
    // Sequence
    compileSeq(className, curClass)

    //curClass.enums.foreach { case(enumName, enumColl) => compileEnum(enumName, enumColl) }

    // Recursive types
    curClass.types.foreach { case (typeName, intClass) => compileArray(intClass) }

    outSrc.puts(s"${topClassName}s.endrecord();")

    outSrc.dec
    outSrc.puts("}")
  }

  def indent: String = "\t"

  def readSeq(curClass: List[String], 
			pos: Option[String], 
			attr: AttrLikeSpec, 
			name: String): Unit = {
    val dataType = attr.dataType
    val dataTypeStr = AwkwardCompiler.dataTypeName(dataType) /*dataType match {
      case st: SwitchType =>
        compileSwitch(name, st)
        s"switch (${expressionType(st.on, name)})"
      case _ =>
        dataTypeName(dataType)*/
    //}
     
    // TODO
    dataType match {
      case ut: UserType =>
        outSrc.puts(s"// Read commands for sequence: ${AwkwardCompiler.type2class(ut.name)}")
        outSrc.puts
      /*case Int1Type(false) | IntMultiType(_, _, _) | BitsType(_) =>
        outSrc.puts(s"${AwkwardCompiler.kaitaiType2NativeType(dataType)} ${name} = ks->read_${dataTypeStr}();")
        outSrc.puts(s"${topClassName}s.field_check($name);")
        outSrc.puts(s"${topClassName}s.integer($name)")
        outSrc.puts*/
      case t: StrFromBytesType =>
        val expr = translator.bytesToStr(AwkwardCompiler.parseExprBytes2(t.bytes, "ks"), Ast.expr.Str(t.encoding))
        
        outSrc.puts(s"${AwkwardCompiler.kaitaiType2NativeType(dataType)} ${name} = $expr")
        outSrc.puts(s"${topClassName}s.field_check(" + "\"" + s"$name" + "\");")
        outSrc.puts(s"${topClassName}s.string($name);")
        outSrc.puts
      case _ =>
        outSrc.puts(s"${AwkwardCompiler.kaitaiType2NativeType(dataType)} ${name} = ks->read_${dataTypeStr}();")
        outSrc.puts(s"${topClassName}s.field_check(" + "\"" + s"$name" + "\");")
        outSrc.puts(s"${topClassName}s.integer($name);")
        outSrc.puts
    	}
    }

  def compileSeq(className: List[String], curClass: ClassSpec): Unit = {
    //tableStart(className, "seq")
    val currSeq: String = s"${AwkwardCompiler.type2class(className)}"  

    CalculateSeqSizes.forEachSeqAttr(curClass, (attr, seqPos, _, _) => {
      attr.id match {
        case NamedIdentifier(name) =>
          readSeq(className, AwkwardCompiler.seqPosToStr(seqPos), attr, name)
        case NumberedIdentifier(n) =>
          readSeq(className, AwkwardCompiler.seqPosToStr(seqPos), attr, s"_${NumberedIdentifier.TEMPLATE}$n")
      }
    })
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
            s"new ${AwkwardCompiler.types2class(t.name)}($addParams$io$addArgs)"
          case SharedPointers =>
            s"std::make_shared<${AwkwardCompiler.types2class(t.name)}>($addParams$io$addArgs)"
          case UniqueAndRawPointers =>
            importListSrc.addSystem("memory")
            // C++14
            //s"std::make_unique<${AwkwardCompiler.types2class(t.name)}>($addParams$io$addArgs)"
            s"std::unique_ptr<${AwkwardCompiler.types2class(t.name)}>(new ${AwkwardCompiler.types2class(t.name)}($addParams$io$addArgs))"
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
          links += ((s"${currentTable}_${attrName}_switch:$portName", AwkwardCompiler.type2class(ut.name) + "__seq", ""))
        case _ =>
          // ignore, no links
      }
    }
  }*/
}

