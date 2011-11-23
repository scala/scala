/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import scala.collection.immutable
import NameTransformer.MODULE_SUFFIX_STRING

trait StdNames extends /*reflect.generic.StdNames with*/ NameManglers { self: SymbolTable =>

  def encode(str: String): TermName = newTermName(NameTransformer.encode(str))

  implicit def stringToTermName(s: String): TermName = newTermName(s)

  /** This should be the first trait in the linearization. */
  trait Keywords {
    private var kws: Set[TermName] = Set()
    private def kw(s: String): TermName = {
      val result = newTermName(s)
      kws = kws + result
      result
    }

    final val ABSTRACTkw: TermName  = kw("abstract")
    final val CASEkw: TermName      = kw("case")
    final val CLASSkw: TermName     = kw("class")
    final val CATCHkw: TermName     = kw("catch")
    final val DEFkw: TermName       = kw("def")
    final val DOkw: TermName        = kw("do")
    final val ELSEkw: TermName      = kw("else")
    final val EXTENDSkw: TermName   = kw("extends")
    final val FALSEkw: TermName     = kw("false")
    final val FINALkw: TermName     = kw("final")
    final val FINALLYkw: TermName   = kw("finally")
    final val FORkw: TermName       = kw("for")
    final val FORSOMEkw: TermName   = kw("forSome")
    final val IFkw: TermName        = kw("if")
    final val IMPLICITkw: TermName  = kw("implicit")
    final val IMPORTkw: TermName    = kw("import")
    final val LAZYkw: TermName      = kw("lazy")
    final val MATCHkw: TermName     = kw("match")
    final val NEWkw: TermName       = kw("new")
    final val NULLkw: TermName      = kw("null")
    final val OBJECTkw: TermName    = kw("object")
    final val OVERRIDEkw: TermName  = kw("override")
    final val PACKAGEkw: TermName   = kw("package")
    final val PRIVATEkw: TermName   = kw("private")
    final val PROTECTEDkw: TermName = kw("protected")
    final val RETURNkw: TermName    = kw("return")
    final val SEALEDkw: TermName    = kw("sealed")
    final val SUPERkw: TermName     = kw("super")
    final val THISkw: TermName      = kw("this")
    final val THROWkw: TermName     = kw("throw")
    final val TRAITkw: TermName     = kw("trait")
    final val TRUEkw: TermName      = kw("true")
    final val TRYkw: TermName       = kw("try")
    final val TYPEkw: TermName      = kw("type")
    final val VALkw: TermName       = kw("val")
    final val VARkw: TermName       = kw("var")
    final val WITHkw: TermName      = kw("with")
    final val WHILEkw: TermName     = kw("while")
    final val YIELDkw: TermName     = kw("yield")
    final val DOTkw: TermName       = kw(".")
    final val USCOREkw: TermName    = kw("_")
    final val COLONkw: TermName     = kw(":")
    final val EQUALSkw: TermName    = kw("=")
    final val ARROWkw: TermName     = kw("=>")
    final val LARROWkw: TermName    = kw("<-")
    final val SUBTYPEkw: TermName   = kw("<:")
    final val VIEWBOUNDkw: TermName = kw("<%")
    final val SUPERTYPEkw: TermName = kw(">:")
    final val HASHkw: TermName      = kw("#")
    final val ATkw: TermName        = kw("@")

    final val keywords = {
      val result = kws.toSet
      kws = null
      result
    }

    final val javaKeywords = new JavaKeywords()
  }

  trait CommonNames /*extends LibraryCommonNames*/ {

    type NameType <: Name
    implicit def createNameType(name: String): NameType

    val EMPTY: NameType              = ""
    val ANON_FUN_NAME: NameType      = "$anonfun"
    val EMPTY_PACKAGE_NAME: NameType = "<empty>"
    val IMPORT: NameType             = "<import>"
    val MODULE_VAR_SUFFIX: NameType  = "$module"
    val ROOT: NameType               = "<root>"
    val PACKAGE: NameType            = "package"

    // value types (and AnyRef) are all used as terms as well
    // as (at least) arguments to the @specialize annotation.
    final val Boolean: NameType = "Boolean"
    final val Byte: NameType    = "Byte"
    final val Char: NameType    = "Char"
    final val Double: NameType  = "Double"
    final val Float: NameType   = "Float"
    final val Int: NameType     = "Int"
    final val Long: NameType    = "Long"
    final val Short: NameType   = "Short"
    final val Unit: NameType    = "Unit"

    final val ScalaValueNames: scala.List[NameType] =
      scala.List(Byte, Char, Short, Int, Long, Float, Double, Boolean, Unit)

    // some types whose companions we utilize
    final val AnyRef: NameType = "AnyRef"
    final val Array: NameType  = "Array"
    final val List: NameType   = "List"
    final val Seq: NameType    = "Seq"
    final val Symbol: NameType = "Symbol"

    // fictions we use as both types and terms
    final val ERROR: NameType    = "<error>"
    final val NO_NAME: NameType  = "<none>"  // formerly NOSYMBOL
    final val WILDCARD: NameType = "_"
  }

  trait TypeNames extends CommonNames {
    final val BYNAME_PARAM_CLASS_NAME: NameType        = "<byname>"
    final val EQUALS_PATTERN_NAME: NameType            = "<equals>"
    final val JAVA_REPEATED_PARAM_CLASS_NAME: NameType = "<repeated...>"
    final val LOCAL_CHILD: NameType                    = "<local child>"
    final val REPEATED_PARAM_CLASS_NAME: NameType      = "<repeated>"
    final val WILDCARD_STAR: NameType                  = "_*"

    final val Any: NameType             = "Any"
    final val AnyVal: NameType          = "AnyVal"
    final val Nothing: NameType         = "Nothing"
    final val Null: NameType            = "Null"
    final val Object: NameType          = "Object"
    final val PartialFunction: NameType = "PartialFunction"
    final val Product: NameType         = "Product"
    final val ScalaObject: NameType     = "ScalaObject"
    final val Serializable: NameType    = "Serializable"
    final val Singleton: NameType       = "Singleton"
    final val String: NameType          = "String"
    final val Throwable: NameType       = "Throwable"

    // Annotation simple names, used in Namer
    final val BeanPropertyAnnot: NameType = "BeanProperty"
    final val BooleanBeanPropertyAnnot: NameType = "BooleanBeanProperty"
    final val bridgeAnnot: NameType = "bridge"

    // Classfile Attributes
    final val AnnotationDefaultATTR: NameType      = "AnnotationDefault"
    final val BridgeATTR: NameType                 = "Bridge"
    final val ClassfileAnnotationATTR: NameType    = "RuntimeInvisibleAnnotations" // RetentionPolicy.CLASS. Currently not used (Apr 2009).
    final val CodeATTR: NameType                   = "Code"
    final val ConstantValueATTR: NameType          = "ConstantValue"
    final val DeprecatedATTR: NameType             = "Deprecated"
    final val ExceptionsATTR: NameType             = "Exceptions"
    final val InnerClassesATTR: NameType           = "InnerClasses"
    final val JacoMetaATTR: NameType               = "JacoMeta"
    final val LineNumberTableATTR: NameType        = "LineNumberTable"
    final val LocalVariableTableATTR: NameType     = "LocalVariableTable"
    final val RuntimeAnnotationATTR: NameType      = "RuntimeVisibleAnnotations"   // RetentionPolicy.RUNTIME
    final val RuntimeParamAnnotationATTR: NameType = "RuntimeVisibleParameterAnnotations" // RetentionPolicy.RUNTIME (annotations on parameters)
    final val ScalaATTR: NameType                  = "Scala"
    final val ScalaSignatureATTR: NameType         = "ScalaSig"
    final val SignatureATTR: NameType              = "Signature"
    final val SourceFileATTR: NameType             = "SourceFile"
    final val SyntheticATTR: NameType              = "Synthetic"
  }


  trait TermNames extends Keywords with CommonNames {
    // Compiler internal names
    val ANYNAME: NameType               = "<anyname>"
    val CONSTRUCTOR: NameType           = "<init>"
    val FAKE_LOCAL_THIS: NameType       = "this$"
    val INITIALIZER: NameType           = CONSTRUCTOR // Is this buying us something?
    val MIXIN_CONSTRUCTOR: NameType     = "$init$"
    val MODULE_INSTANCE_FIELD: NameType = NameTransformer.MODULE_INSTANCE_NAME  // "MODULE$"
    val OUTER: NameType                 = "$outer"
    val OUTER_LOCAL: NameType           = "$outer " // note the space
    val OUTER_SYNTH: NameType           = "<outer>" // emitted by virtual pattern matcher, replaced by outer accessor in explicitouter
    val SELF: NameType                  = "$this"
    val SPECIALIZED_INSTANCE: NameType  = "specInstance$"
    val STAR: NameType                  = "*"
    val THIS: NameType                  = "_$this"
    val SELECTOR_DUMMY: NameType        = "<unapply-selector>"

    final val Nil: NameType             = "Nil"
    final val Predef: NameType          = "Predef"
    final val ScalaRunTime: NameType    = "ScalaRunTime"
    final val Some: NameType            = "Some"

    // Compiler utilized names
    // val productElementName: NameType = "productElementName"
    val TYPE_ : NameType           = "TYPE"
    val add_ : NameType            = "add"
    val anyValClass: NameType      = "anyValClass"
    val append: NameType           = "append"
    val apply: NameType            = "apply"
    val arrayValue: NameType       = "arrayValue"
    val arraycopy: NameType        = "arraycopy"
    val asInstanceOf_ : NameType   = "asInstanceOf"
    val assert_ : NameType         = "assert"
    val assume_ : NameType         = "assume"
    val box: NameType              = "box"
    val bytes: NameType            = "bytes"
    val canEqual_ : NameType       = "canEqual"
    val checkInitialized: NameType = "checkInitialized"
    val classOf: NameType          = "classOf"
    val clone_ : NameType          = if (forMSIL) "MemberwiseClone" else "clone" // sn.OClone causes checkinit failure
    val conforms: NameType         = "conforms"
    val copy: NameType             = "copy"
    val delayedInit: NameType      = "delayedInit"
    val delayedInitArg: NameType   = "delayedInit$body"
    val drop: NameType             = "drop"
    val elem: NameType             = "elem"
    val eq: NameType               = "eq"
    val equals_ : NameType         = if (forMSIL) "Equals" else "equals"
    val error: NameType            = "error"
    val ex: NameType               = "ex"
    val false_ : NameType          = "false"
    val filter: NameType           = "filter"
    val finalize_ : NameType       = if (forMSIL) "Finalize" else "finalize"
    val find_ : NameType           = "find"
    val flatMap: NameType          = "flatMap"
    val foreach: NameType          = "foreach"
    val formatted: NameType        = "formatted"
    val genericArrayOps: NameType  = "genericArrayOps"
    val get: NameType              = "get"
    val hasNext: NameType          = "hasNext"
    val hashCode_ : NameType       = if (forMSIL) "GetHashCode" else "hashCode"
    val hash_ : NameType           = "hash"
    val head: NameType             = "head"
    val identity: NameType         = "identity"
    val inlinedEquals: NameType    = "inlinedEquals"
    val applyDynamic: NameType     = "applyDynamic"
    val isArray: NameType          = "isArray"
    val isDefinedAt: NameType      = "isDefinedAt"
    val _isDefinedAt: NameType     = "_isDefinedAt"
    val isEmpty: NameType          = "isEmpty"
    val isInstanceOf_ : NameType   = "isInstanceOf"
    val java: NameType             = "java"
    val lang: NameType             = "lang"
    val length: NameType           = "length"
    val lengthCompare: NameType    = "lengthCompare"
    val lift_ : NameType           = "lift"
    val macro_ : NameType          = "macro"
    val main: NameType             = "main"
    val map: NameType              = "map"
    val missingCase: NameType      = "missingCase"
    val ne: NameType               = "ne"
    val newArray: NameType         = "newArray"
    val next: NameType             = "next"
    val notifyAll_ : NameType      = "notifyAll"
    val notify_ : NameType         = "notify"
    val null_ : NameType           = "null"
    val ofDim: NameType            = "ofDim"
    val productArity: NameType     = "productArity"
    val productElement: NameType   = "productElement"
    val productIterator: NameType  = "productIterator"
    val productPrefix: NameType    = "productPrefix"
    val readResolve: NameType      = "readResolve"
    val runOrElse: NameType        = "runOrElse"
    val sameElements: NameType     = "sameElements"
    val scala_ : NameType          = "scala"
    val self: NameType             = "self"
    val setAccessible: NameType    = "setAccessible"
    val synchronized_ : NameType   = "synchronized"
    val tail: NameType             = "tail"
    val this_ : NameType           = "this"
    val throw_ : NameType          = "throw"
    val toArray: NameType          = "toArray"
    val toList: NameType           = "toList"
    val toSeq: NameType            = "toSeq"
    val toString_ : NameType       = if (forMSIL) "ToString" else "toString"
    val true_ : NameType           = "true"
    val unapply: NameType          = "unapply"
    val unapplySeq: NameType       = "unapplySeq"
    val unbox: NameType            = "unbox"
    val update: NameType           = "update"
    val value: NameType            = "value"
    val view_ : NameType           = "view"
    val wait_ : NameType           = "wait"
    val withFilter: NameType       = "withFilter"
    val wrapRefArray: NameType     = "wrapRefArray"
    val zip: NameType              = "zip"

    // unencoded operators
    object raw {
      final val AMP  : NameType  = "&"
      final val BANG : NameType  = "!"
      final val BAR  : NameType  = "|"
      final val DOLLAR: NameType = "$"
      final val GE: NameType     = ">="
      final val LE: NameType     = "<="
      final val MINUS: NameType  = "-"
      final val NE: NameType     = "!="
      final val PLUS : NameType  = "+"
      final val SLASH: NameType  = "/"
      final val STAR : NameType  = "*"
      final val TILDE: NameType  = "~"

      final val isUnary: Set[Name] = Set(MINUS, PLUS, TILDE, BANG)
    }

    // value-conversion methods
    val toByte: NameType   = "toByte"
    val toShort: NameType  = "toShort"
    val toChar: NameType   = "toChar"
    val toInt: NameType    = "toInt"
    val toLong: NameType   = "toLong"
    val toFloat: NameType  = "toFloat"
    val toDouble: NameType = "toDouble"
  }

  object tpnme extends TypeNames /*with LibraryTypeNames*/ with TypeNameMangling {
    type NameType = TypeName
    implicit def createNameType(name: String): TypeName = newTypeName(name)

    val REFINE_CLASS_NAME: NameType  = "<refinement>"
    val ANON_CLASS_NAME: NameType    = "$anon"
  }

  val javanme = nme.javaKeywords

  object nme extends TermNames /*with LibraryTermNames*/ with TermNameMangling {
    type NameType = TermName
    def createNameType(name: String): TermName = newTermName(name)

    /** Translate a String into a list of simple TypeNames and TermNames.
     *  In all segments before the last, type/term is determined by whether
     *  the following separator char is '.' or '#'.  In the last segment,
     *  the argument "assumeTerm" determines it.  Examples:
     *
     *  package foo {
     *    object Lorax { object Wog ; class Wog }
     *    class Lorax  { object Zax ; class Zax }
     *  }
     *
     *  f("foo.Lorax", true)   == List("foo": Term, "Lorax": Term) // object Lorax
     *  f("foo.Lorax", false)  == List("foo": Term, "Lorax": Type) // class Lorax
     *  f("Lorax.Wog", true)   == List("Lorax": Term, "Wog": Term) // object Wog
     *  f("Lorax.Wog", false)  == List("Lorax": Term, "Wog": Type) // class Wog
     *  f("Lorax#Zax", true)   == List("Lorax": Type, "Zax": Term) // object Zax
     *  f("Lorax#Zax", false)  == List("Lorax": Type, "Zax": Type) // class Zax
     *
     *  Note that in actual scala syntax you cannot refer to object Zax without an
     *  instance of Lorax, so Lorax#Zax could only mean the type.  One might think
     *  that Lorax#Zax.type would work, but this is not accepted by the parser.
     *  For the purposes of referencing that object, the syntax is allowed.
     */
    def segments(name: String, assumeTerm: Boolean): List[Name] = {
      def mkName(str: String, term: Boolean): Name =
        if (term) newTermName(str) else newTypeName(str)

      name.indexWhere(ch => ch == '.' || ch == '#') match {
        // it's the last segment: the parameter tells us whether type or term
        case -1     => if (name == "") scala.Nil else scala.List(mkName(name, assumeTerm))
        // otherwise, we can tell based on whether '#' or '.' is the following char.
        case idx    =>
          val (simple, div, rest) = (name take idx, name charAt idx, name drop (idx + 1))
          mkName(simple, div == '.') :: segments(rest, assumeTerm)
      }
    }

    def newBitmapName(bitmapPrefix: Name, n: Int) = bitmapPrefix append ("" + n)

    val BITMAP_PREFIX: String                = "bitmap$"
    val BITMAP_NORMAL: NameType              = BITMAP_PREFIX + ""           // initialization bitmap for public/protected lazy vals
    val BITMAP_TRANSIENT: NameType           = BITMAP_PREFIX + "trans$"     // initialization bitmap for transient lazy vals
    val BITMAP_PRIVATE: NameType             = BITMAP_PREFIX + "priv$"      // initialization bitmap for private lazy vals
    val BITMAP_CHECKINIT: NameType           = BITMAP_PREFIX + "init$"      // initialization bitmap for checkinit values
    val BITMAP_CHECKINIT_TRANSIENT: NameType = BITMAP_PREFIX + "inittrans$" // initialization bitmap for transient checkinit values

    /** The expanded name of `name` relative to this class `base` with given `separator`
     */
    def expandedName(name: TermName, base: Symbol, separator: String = EXPAND_SEPARATOR_STRING): TermName =
      newTermName(base.fullName('$') + separator + name)

    def moduleVarName(name: TermName): TermName = newTermName("" + name + MODULE_VAR_SUFFIX)

    val EXPAND_SEPARATOR_STRING = "$$"
    val LOCAL_SUFFIX_STRING     = " "
    val ROOTPKG: TermName       = "_root_"

    /** Base strings from which synthetic names are derived. */
    val CHECK_IF_REFUTABLE_STRING   = "check$ifrefutable$"
    val DEFAULT_GETTER_STRING       = "$default$"
    val DO_WHILE_PREFIX             = "doWhile$"
    val EQEQ_LOCAL_VAR              = "eqEqTemp$"
    val EVIDENCE_PARAM_PREFIX       = "evidence$"
    val EXCEPTION_RESULT_PREFIX     = "exceptionResult"
    val INTERPRETER_IMPORT_WRAPPER  = "$iw"
    val INTERPRETER_LINE_PREFIX     = "line"
    val INTERPRETER_VAR_PREFIX      = "res"
    val INTERPRETER_WRAPPER_SUFFIX  = "$object"
    val WHILE_PREFIX                = "while$"

    def getCause   = sn.GetCause
    def getClass_  = sn.GetClass
    def getMethod_ = sn.GetMethod
    def invoke_    = sn.Invoke

    val ADD      = encode("+")
    val AND      = encode("&")
    val ASR      = encode(">>")
    val DIV      = encode("/")
    val EQ       = encode("==")
    val EQL      = encode("=")
    val GE       = encode(">=")
    val GT       = encode(">")
    val HASHHASH = encode("##")
    val LE       = encode("<=")
    val LSL      = encode("<<")
    val LSR      = encode(">>>")
    val LT       = encode("<")
    val MINUS    = encode("-")
    val MOD      = encode("%")
    val MUL      = encode("*")
    val NE       = encode("!=")
    val OR       = encode("|")
    val PLUS     = encode("+")
    val SUB      = encode("-")
    val XOR      = encode("^")
    val ZAND     = encode("&&")
    val ZOR      = encode("||")

    // unary operators
    val UNARY_~ = encode("unary_~")
    val UNARY_+ = encode("unary_+")
    val UNARY_- = encode("unary_-")
    val UNARY_! = encode("unary_!")
  }

  abstract class SymbolNames {
    protected implicit def stringToTypeName(s: String): TypeName = newTypeName(s)

    val BeanProperty        : TypeName
    val BooleanBeanProperty : TypeName
    val BoxedBoolean        : TypeName
    val BoxedCharacter      : TypeName
    val BoxedNumber         : TypeName
    val Class               : TypeName
    val Code                : TypeName
    val Delegate            : TypeName
    val IOOBException       : TypeName // IndexOutOfBoundsException
    val InvTargetException  : TypeName // InvocationTargetException
    val JavaSerializable    : TypeName
    val MethodAsObject      : TypeName
    val NPException         : TypeName // NullPointerException
    val Object              : TypeName
    val String              : TypeName
    val Throwable           : TypeName
    val ValueType           : TypeName

    val ForName             : TermName
    val GetCause            : TermName
    val GetClass            : TermName
    val GetMethod           : TermName
    val Invoke              : TermName
    val JavaLang            : TermName

    val Boxed: immutable.Map[TypeName, TypeName]
  }

  class JavaKeywords {
    private var kws: Set[TermName] = Set()
    private def kw(s: String): TermName = {
      val result = newTermName(s)
      kws = kws + result
      result
    }

    final val ABSTRACTkw: TermName     = kw("abstract")
    final val ASSERTkw: TermName       = kw("assert")
    final val BOOLEANkw: TermName      = kw("boolean")
    final val BREAKkw: TermName        = kw("break")
    final val BYTEkw: TermName         = kw("byte")
    final val CASEkw: TermName         = kw("case")
    final val CATCHkw: TermName        = kw("catch")
    final val CHARkw: TermName         = kw("char")
    final val CLASSkw: TermName        = kw("class")
    final val CONSTkw: TermName        = kw("const")
    final val CONTINUEkw: TermName     = kw("continue")
    final val DEFAULTkw: TermName      = kw("default")
    final val DOkw: TermName           = kw("do")
    final val DOUBLEkw: TermName       = kw("double")
    final val ELSEkw: TermName         = kw("else")
    final val ENUMkw: TermName         = kw("enum")
    final val EXTENDSkw: TermName      = kw("extends")
    final val FINALkw: TermName        = kw("final")
    final val FINALLYkw: TermName      = kw("finally")
    final val FLOATkw: TermName        = kw("float")
    final val FORkw: TermName          = kw("for")
    final val IFkw: TermName           = kw("if")
    final val GOTOkw: TermName         = kw("goto")
    final val IMPLEMENTSkw: TermName   = kw("implements")
    final val IMPORTkw: TermName       = kw("import")
    final val INSTANCEOFkw: TermName   = kw("instanceof")
    final val INTkw: TermName          = kw("int")
    final val INTERFACEkw: TermName    = kw("interface")
    final val LONGkw: TermName         = kw("long")
    final val NATIVEkw: TermName       = kw("native")
    final val NEWkw: TermName          = kw("new")
    final val PACKAGEkw: TermName      = kw("package")
    final val PRIVATEkw: TermName      = kw("private")
    final val PROTECTEDkw: TermName    = kw("protected")
    final val PUBLICkw: TermName       = kw("public")
    final val RETURNkw: TermName       = kw("return")
    final val SHORTkw: TermName        = kw("short")
    final val STATICkw: TermName       = kw("static")
    final val STRICTFPkw: TermName     = kw("strictfp")
    final val SUPERkw: TermName        = kw("super")
    final val SWITCHkw: TermName       = kw("switch")
    final val SYNCHRONIZEDkw: TermName = kw("synchronized")
    final val THISkw: TermName         = kw("this")
    final val THROWkw: TermName        = kw("throw")
    final val THROWSkw: TermName       = kw("throws")
    final val TRANSIENTkw: TermName    = kw("transient")
    final val TRYkw: TermName          = kw("try")
    final val VOIDkw: TermName         = kw("void")
    final val VOLATILEkw: TermName     = kw("volatile")
    final val WHILEkw: TermName        = kw("while")

    final val keywords = {
      val result = kws.toSet
      kws = null
      result
    }
  }

  private abstract class JavaNames extends SymbolNames {
    final val BoxedBoolean: TypeName       = "java.lang.Boolean"
    final val BoxedByte: TypeName          = "java.lang.Byte"
    final val BoxedCharacter: TypeName     = "java.lang.Character"
    final val BoxedDouble: TypeName        = "java.lang.Double"
    final val BoxedFloat: TypeName         = "java.lang.Float"
    final val BoxedInteger: TypeName       = "java.lang.Integer"
    final val BoxedLong: TypeName          = "java.lang.Long"
    final val BoxedNumber: TypeName        = "java.lang.Number"
    final val BoxedShort: TypeName         = "java.lang.Short"
    final val Class: TypeName              = "java.lang.Class"
    final val Delegate: TypeName           = tpnme.NO_NAME
    final val IOOBException: TypeName      = "java.lang.IndexOutOfBoundsException"
    final val InvTargetException: TypeName = "java.lang.reflect.InvocationTargetException"
    final val MethodAsObject: TypeName     = "java.lang.reflect.Method"
    final val NPException: TypeName        = "java.lang.NullPointerException"
    final val Object: TypeName             = "java.lang.Object"
    final val String: TypeName             = "java.lang.String"
    final val Throwable: TypeName          = "java.lang.Throwable"
    final val ValueType: TypeName          = tpnme.NO_NAME

    final val ForName: TermName   = "forName"
    final val GetCause: TermName  = "getCause"
    final val GetClass: TermName  = "getClass"
    final val GetMethod: TermName = "getMethod"
    final val Invoke: TermName    = "invoke"
    final val JavaLang: TermName  = "java.lang"

    val Boxed = immutable.Map[TypeName, TypeName](
      tpnme.Boolean -> BoxedBoolean,
      tpnme.Byte    -> BoxedByte,
      tpnme.Char    -> BoxedCharacter,
      tpnme.Short   -> BoxedShort,
      tpnme.Int     -> BoxedInteger,
      tpnme.Long    -> BoxedLong,
      tpnme.Float   -> BoxedFloat,
      tpnme.Double  -> BoxedDouble
    )
  }

  private class MSILNames extends SymbolNames {
    final val BeanProperty: TypeName        = tpnme.NO_NAME
    final val BooleanBeanProperty: TypeName = tpnme.NO_NAME
    final val BoxedBoolean: TypeName        = "System.IConvertible"
    final val BoxedCharacter: TypeName      = "System.IConvertible"
    final val BoxedNumber: TypeName         = "System.IConvertible"
    final val Class: TypeName               = "System.Type"
    final val Code: TypeName                = tpnme.NO_NAME
    final val Delegate: TypeName            = "System.MulticastDelegate"
    final val IOOBException: TypeName       = "System.IndexOutOfRangeException"
    final val InvTargetException: TypeName  = "System.Reflection.TargetInvocationException"
    final val JavaSerializable: TypeName    = tpnme.NO_NAME
    final val MethodAsObject: TypeName      = "System.Reflection.MethodInfo"
    final val NPException: TypeName         = "System.NullReferenceException"
    final val Object: TypeName              = "System.Object"
    final val String: TypeName              = "System.String"
    final val Throwable: TypeName           = "System.Exception"
    final val ValueType: TypeName           = "System.ValueType"

    final val ForName: TermName   = "GetType"
    final val GetCause: TermName  = "InnerException" /* System.Reflection.TargetInvocationException.InnerException */
    final val GetClass: TermName  = "GetType"
    final val GetMethod: TermName = "GetMethod"
    final val Invoke: TermName    = "Invoke"
    final val JavaLang: TermName  = "System"

    val Boxed = immutable.Map[TypeName, TypeName](
      tpnme.Boolean -> "System.Boolean",
      tpnme.Byte    -> "System.SByte", // a scala.Byte is signed and a System.SByte too (unlike a System.Byte)
      tpnme.Char    -> "System.Char",
      tpnme.Short   -> "System.Int16",
      tpnme.Int     -> "System.Int32",
      tpnme.Long    -> "System.Int64",
      tpnme.Float   -> "System.Single",
      tpnme.Double  -> "System.Double"
    )
  }

  private class J2SENames extends JavaNames {
    final val BeanProperty: TypeName        = "scala.beans.BeanProperty"
    final val BooleanBeanProperty: TypeName = "scala.beans.BooleanBeanProperty"
    final val Code: TypeName                = "scala.reflect.Code"
    final val JavaSerializable: TypeName    = "java.io.Serializable"
  }

  lazy val sn: SymbolNames =
    if (forMSIL) new MSILNames
    else new J2SENames
}
