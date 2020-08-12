package io.kaitai.struct.languages

import io.kaitai.struct.CppRuntimeConfig._
import io.kaitai.struct._
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.{CppTranslator, TypeDetector}

import scala.collection.mutable.ListBuffer

class AwkwardCompiler(
  //override val typeProvider: ClassTypeProvider, 
  typeProvider: ClassTypeProvider,
  //classSpecs: ClassSpecs, 
  //val topClass: ClassSpec, 
  config: RuntimeConfig) 
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with AllocateAndStoreIO
    with FixedContentsUsingArrayByteLiteral
    with UniversalDoc
    with SwitchIfOps
    with EveryReadIsExpression {

  import AwkwardCompiler._

  val importListSrc = new CppImportList
  val importListHdr = new CppImportList

  //val translator = new CppTranslator(provider, importListSrc, importListHdr, config)
  override val translator: translators.CppTranslator = new CppTranslator(typeProvider, importListSrc, importListHdr, config)

  def parseExprBytes2(dataType: BytesType, io: String): String = { parseExprBytes(dataType, io) }

  override def expression(e: Ast.expr): String = translator.translate(e) 

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io->read_${t.apiCall(defEndian)}()"
      case blt: BytesLimitType =>
        s"$io->read_bytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io->read_bytes_full()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io->read_bytes_term($terminator, $include, $consume, $eosError)"
      case BitsType1(bitEndian) =>
        s"$io->read_bits_int_${bitEndian.toSuffix}(1)"
      case BitsType(width: Int, bitEndian) =>
        s"$io->read_bits_int_${bitEndian.toSuffix}($width)"
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
            s"case: shared pointers "//s"std::make_shared<${AwkwardCompiler.types2class(t.name)}>($addParams$io$addArgs)"
          case UniqueAndRawPointers =>
            s"case: unique and raw pointers" //importListSrc.addSystem("memory")
            // C++14
            //s"std::make_unique<${AwkwardCompiler.types2class(t.name)}>($addParams$io$addArgs)"
            s"std::unique_ptr<${AwkwardCompiler.types2class(t.name)}>(new ${AwkwardCompiler.types2class(t.name)}($addParams$io$addArgs))"
        }
    }
  }

/* Group of handleAssignment functions*/
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit = {
    //outSrc.puts(s"${privateMemberName(id)}->push_back(${stdMoveWrap(expr)});")
  }

  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit = {
    //outSrc.puts(s"${privateMemberName(id)}->push_back(${stdMoveWrap(expr)});")
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    /*val (typeDecl, tempVar) = if (isRaw) {
      ("std::string ", translator.doName(Identifier.ITERATOR2))
    } else {
      ("", translator.doName(Identifier.ITERATOR))
    }
    val (wrappedTempVar, rawPtrExpr) = if (config.cppConfig.pointers == UniqueAndRawPointers) {
      expr match {
        case ReStdUniquePtr(cppClass, innerExpr) =>
          (s"std::move(std::unique_ptr<$cppClass>($tempVar))", innerExpr)
        case _ =>
          (tempVar, expr)
      }
    } else {
      (tempVar, expr)
    }

    outSrc.puts(s"$typeDecl$tempVar = $rawPtrExpr;")

    outSrc.puts(s"${privateMemberName(id)}->push_back($wrappedTempVar);")
    */
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit = {
    //outSrc.puts(s"${privateMemberName(id)} = $expr;")
  }

// Unimplemented methods from AwkwardCompiler

  override def allocateIO(id: format.Identifier,rep: format.RepeatSpec): String = {""}

  // Members declared in languages.components.EveryReadIsExpression
  override def bytesPadTermExpr(expr0: String,padRight: Option[Int],terminator: Option[Int],include: Boolean): String = {""}
  //override def parseExpr(dataType: datatype.DataType,assignType: datatype.DataType,io: String,defEndian: Option[datatype.FixedEndian]): String = {""}
  
  // Members declared in languages.components.ExceptionNames
  override def ksErrorName(err: datatype.KSError): String = {""}
  
  // Members declared in languages.components.FixedContentsUsingArrayByteLiteral
  override def attrFixedContentsParse(attrName: format.Identifier,contents: String): Unit = {}
  
  // Members declared in languages.components.LanguageCompiler
  override def alignToByte(io: String): Unit = {}
  override def attrParseHybrid(leProc: () => Unit,beProc: () => Unit): Unit = {}
  override def attrProcess(proc: format.ProcessExpr,varSrc: format.Identifier,varDest: format.Identifier,rep: format.RepeatSpec): Unit = {}
  override def attributeDeclaration(attrName: format.Identifier,attrType: datatype.DataType,isNullable: Boolean): Unit = {}
  override def attributeReader(attrName: format.Identifier,attrType: datatype.DataType,isNullable: Boolean): Unit = {}
  override def classConstructorFooter: Unit = {}
  override def classConstructorHeader(name: List[String],parentType: datatype.DataType,rootClassName: List[String],isHybrid: Boolean,params: List[format.ParamDefSpec]): Unit = {}
  override def classFooter(name: List[String]): Unit = {}
  override def classHeader(name: List[String]): Unit = {}
  override def condIfFooter(expr: exprlang.Ast.expr): Unit = {}
  override def condIfHeader(expr: exprlang.Ast.expr): Unit = {}
  override def condRepeatEosFooter: Unit = {}
  override def condRepeatEosHeader(id: format.Identifier,io: String,dataType: datatype.DataType,needRaw: datatype.NeedRaw): Unit = {}
  override def condRepeatExprFooter: Unit = {}
  override def condRepeatExprHeader(id: format.Identifier,io: String,dataType: datatype.DataType,needRaw: datatype.NeedRaw,repeatExpr: exprlang.Ast.expr): Unit = {}
  override def condRepeatUntilFooter(id: format.Identifier,io: String,dataType: datatype.DataType,needRaw: datatype.NeedRaw,repeatExpr: exprlang.Ast.expr): Unit = {}
  override def condRepeatUntilHeader(id: format.Identifier,io: String,dataType: datatype.DataType,needRaw: datatype.NeedRaw,repeatExpr: exprlang.Ast.expr): Unit = {}
  override def enumDeclaration(curClass: List[String],enumName: String,enumColl: Seq[(Long, format.EnumValueSpec)]): Unit = {}
  override def fileHeader(topClassName: String): Unit = {}
  override def indent: String = {""}
  override def instanceCheckCacheAndReturn(instName: format.InstanceIdentifier,dataType: datatype.DataType): Unit = {}
  override def instanceFooter: Unit = {}
  override def instanceHeader(className: List[String],instName: format.InstanceIdentifier,dataType: datatype.DataType,isNullable: Boolean): Unit = {}
  override def instanceReturn(instName: format.InstanceIdentifier,attrType: datatype.DataType): Unit = {}
  override def outFileName(topClassName: String): String = {""}
  override def popPos(io: String): Unit = {}
  override def pushPos(io: String): Unit = {}
  override def readFooter(): Unit = {}
  override def readHeader(endian: Option[datatype.FixedEndian],isEmpty: Boolean): Unit = {}
  override def results(topClass: format.ClassSpec): Map[String,String] = { Map() }
  override def runRead(name: List[String]): Unit = {}
  override def runReadCalc(): Unit = {}
  override def seek(io: String,pos: exprlang.Ast.expr): Unit = {}
  override def type2class(className: String): String = {""}
  override def useIO(ioEx: exprlang.Ast.expr): String = {""}

  // Members declared in languages.components.ObjectOrientedLanguage
  override def idToStr(id: format.Identifier): String = {""}
  override def localTemporaryName(id: format.Identifier): String = {""}
  override def privateMemberName(id: format.Identifier): String = {""}
  override def publicMemberName(id: format.Identifier): String = {""}

  // Members declared in languages.components.SwitchIfOps
  override def switchIfCaseEnd(): Unit = {}
  override def switchIfCaseStart(condition: exprlang.Ast.expr): Unit = {}
  override def switchIfElseStart(): Unit = {}
  override def switchIfEnd(): Unit = {}
  override def switchIfStart(id: format.Identifier,on: exprlang.Ast.expr,onType: datatype.DataType): Unit = {}
  override def switchRequiresIfs(onType: datatype.DataType): Boolean = {true}

  // Members declared in languages.components.SwitchOps
  override def switchCaseEnd(): Unit = {}
  override def switchCaseStart(condition: exprlang.Ast.expr): Unit = {}
  override def switchElseStart(): Unit = {}
  override def switchEnd(): Unit = {}
  override def switchStart(id: format.Identifier,on: exprlang.Ast.expr): Unit = {}

  // Members declared in languages.components.UniversalDoc
  override def universalDoc(doc: format.DocSpec): Unit = {}

}

// extension of LanguageCompilerStatic
object AwkwardCompiler extends LanguageCompilerStatic {
  // FIXME: Unused, should be probably separated from LanguageCompilerStatic
  // override def getCompiler(
  //   tp: ClassTypeProvider,
  //   classSpecs: ClassSpecs, 
  //   topClass: ClassSpec,
  //   config: RuntimeConfig
  // ): LanguageCompiler = new AwkwardCompiler(tp, classSpecs, topClass, config)
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new AwkwardCompiler(tp, config)

  def type2class(name: List[String]) = name.last
  def type2display(name: List[String]) = name.map(Utils.upperCamelCase).mkString("::")

  def kstructName = "kaitai::kstruct"
  def kstreamName = "kaitai::kstream"

  //def parseExprBytes2(dataType: BytesType, io:String): String = {"bababooey"}

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

      case BitsType(_, _) => "uint64_t"

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
      case BitsType(width, bitEndian) => s"b$width"
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