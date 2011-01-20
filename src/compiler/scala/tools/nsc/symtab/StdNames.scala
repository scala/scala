/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab

import scala.collection.immutable

trait StdNames extends reflect.generic.StdNames with NameManglers {
  self: SymbolTable =>

  trait CompilerCommonNames extends LibraryCommonNames {
    // value types are all used as terms as well
    final val Boolean: NameType = "Boolean"
    final val Byte: NameType    = "Byte"
    final val Char: NameType    = "Char"
    final val Double: NameType  = "Double"
    final val Float: NameType   = "Float"
    final val Int: NameType     = "Int"
    final val Long: NameType    = "Long"
    final val Short: NameType   = "Short"
    final val Unit: NameType    = "Unit"

    // types whose companions we utilize
    final val Array: NameType  = "Array"
    final val List: NameType   = "List"
    final val Seq: NameType    = "Seq"
    final val Symbol: NameType = "Symbol"

    // fictions we use as both types and terms
    final val ERROR: NameType    = "<error>"
    final val NO_NAME: NameType  = "<none>"  // formerly NOSYMBOL
    final val WILDCARD: NameType = "_"
  }

  trait CompilerTypeNames extends CompilerCommonNames {
    final val BYNAME_PARAM_CLASS_NAME: NameType        = "<byname>"
    final val EQUALS_PATTERN_NAME: NameType            = "<equals>"
    final val JAVA_REPEATED_PARAM_CLASS_NAME: NameType = "<repeated...>"
    final val LOCAL_CHILD: NameType                    = "<local child>"
    final val REPEATED_PARAM_CLASS_NAME: NameType      = "<repeated>"
    final val WILDCARD_STAR: NameType                  = "_*"

    final val Any: NameType             = "Any"
    final val AnyRef: NameType          = "AnyRef"
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

    // Annotation types
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

  trait CompilerTermNames extends CompilerCommonNames {
    // Scala keywords
    final val ABSTRACTkw: NameType  = "abstract"
    final val CASEkw: NameType      = "case"
    final val CLASSkw: NameType     = "class"
    final val CATCHkw: NameType     = "catch"
    final val DEFkw: NameType       = "def"
    final val DOkw: NameType        = "do"
    final val ELSEkw: NameType      = "else"
    final val EXTENDSkw: NameType   = "extends"
    final val FALSEkw: NameType     = "false"
    final val FINALkw: NameType     = "final"
    final val FINALLYkw: NameType   = "finally"
    final val FORkw: NameType       = "for"
    final val FORSOMEkw: NameType   = "forSome"
    final val IFkw: NameType        = "if"
    final val IMPLICITkw: NameType  = "implicit"
    final val IMPORTkw: NameType    = "import"
    final val LAZYkw: NameType      = "lazy"
    final val MATCHkw: NameType     = "match"
    final val NEWkw: NameType       = "new"
    final val NULLkw: NameType      = "null"
    final val OBJECTkw: NameType    = "object"
    final val OVERRIDEkw: NameType  = "override"
    final val PACKAGEkw: NameType   = "package"
    final val PRIVATEkw: NameType   = "private"
    final val PROTECTEDkw: NameType = "protected"
    final val RETURNkw: NameType    = "return"
    final val REQUIRESkw: NameType  = "requires"
    final val SEALEDkw: NameType    = "sealed"
    final val SUPERkw: NameType     = "super"
    final val THISkw: NameType      = "this"
    final val THROWkw: NameType     = "throw"
    final val TRAITkw: NameType     = "trait"
    final val TRUEkw: NameType      = "true"
    final val TRYkw: NameType       = "try"
    final val TYPEkw: NameType      = "type"
    final val VALkw: NameType       = "val"
    final val VARkw: NameType       = "var"
    final val WITHkw: NameType      = "with"
    final val WHILEkw: NameType     = "while"
    final val YIELDkw: NameType     = "yield"
    final val DOTkw: NameType       = "."
    final val USCOREkw: NameType    = "_"
    final val COLONkw: NameType     = ":"
    final val EQUALSkw: NameType    = "="
    final val ARROWkw: NameType     = "=>"
    final val LARROWkw: NameType    = "<-"
    final val SUBTYPEkw: NameType   = "<:"
    final val VIEWBOUNDkw: NameType = "<%"
    final val SUPERTYPEkw: NameType = ">:"
    final val HASHkw: NameType      = "#"
    final val ATkw: NameType        = "@"

    // Compiler internal names
    val ANYNAME: NameType               = "<anyname>"
    val CONSTRUCTOR: NameType           = "<init>"
    val FAKE_LOCAL_THIS: NameType       = "this$"
    val INITIALIZER: NameType           = CONSTRUCTOR // Is this buying us something?
    val MIXIN_CONSTRUCTOR: NameType     = "$init$"
    val MODULE_INSTANCE_FIELD: NameType = "MODULE$"
    val OUTER: NameType                 = "$outer"
    val OUTER_LOCAL: NameType           = "$outer " // note the space
    val SELF: NameType                  = "$this"
    val SPECIALIZED_INSTANCE: NameType  = "specInstance$"
    val STAR: NameType                  = "*"
    val THIS: NameType                  = "_$this"

    final val Nil: NameType             = "Nil"
    final val Predef: NameType          = "Predef"
    final val ScalaRunTime: NameType    = "ScalaRunTime"
    final val Some: NameType            = "Some"

    // Compiler utilized names
    // val productElementName: NameType = "productElementName"
    val TYPE_ : NameType           = "TYPE"
    val add_ : NameType            = "add"
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
    val clone_ : NameType          = "clone"
    val conforms: NameType         = "conforms"
    val copy: NameType             = "copy"
    val delayedInit: NameType      = "delayedInit"
    val delayedInitArg: NameType   = "delayedInit$body"
    val dottype: NameType          = ".type"
    val drop: NameType             = "drop"
    val elem: NameType             = "elem"
    val eq: NameType               = "eq"
    val equals_ : NameType         = "equals"
    val error: NameType            = "error"
    val ex: NameType               = "ex"
    val false_ : NameType          = "false"
    val filter: NameType           = "filter"
    val finalize_ : NameType       = "finalize"
    val find_ : NameType           = "find"
    val flatMap: NameType          = "flatMap"
    val foreach: NameType          = "foreach"
    val genericArrayOps: NameType  = "genericArrayOps"
    val get: NameType              = "get"
    val hasNext: NameType          = "hasNext"
    val hashCode_ : NameType       = "hashCode"
    val hash_ : NameType           = "hash"
    val head: NameType             = "head"
    val identity: NameType         = "identity"
    val inlinedEquals: NameType    = "inlinedEquals"
    val invokeDynamic: NameType    = "invokeDynamic"
    val isArray: NameType          = "isArray"
    val isDefinedAt: NameType      = "isDefinedAt"
    val isEmpty: NameType          = "isEmpty"
    val isInstanceOf_ : NameType   = "isInstanceOf"
    val java: NameType             = "java"
    val lang: NameType             = "lang"
    val length: NameType           = "length"
    val lengthCompare: NameType    = "lengthCompare"
    val lift_ : NameType           = "lift"
    val main: NameType             = "main"
    val map: NameType              = "map"
    val ne: NameType               = "ne"
    val newArray: NameType         = "newArray"
    val next: NameType             = "next"
    val notifyAll_ : NameType      = "notifyAll"
    val notify_ : NameType         = "notify"
    val null_ : NameType           = "null"
    val ofDim: NameType            = "ofDim"
    val productArity: NameType     = "productArity"
    val productElement: NameType   = "productElement"
    val productPrefix: NameType    = "productPrefix"
    val readResolve: NameType      = "readResolve"
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
    val toString_ : NameType       = "toString"
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

  object tpnme extends CompilerTypeNames with LibraryTypeNames {
    type NameType = TypeName
    implicit def createNameType(name: String): TypeName = newTypeName(name)
  }

  object nme extends CompilerTermNames with LibraryTermNames with NameMangling {
    type NameType = TermName
    implicit def createNameType(name: String): TermName = newTermName(name)

    final val keywords = Set[TermName](
      ABSTRACTkw, CASEkw, CLASSkw, CATCHkw, DEFkw, DOkw, ELSEkw,
      EXTENDSkw, FALSEkw, FINALkw, FINALLYkw, FORkw, FORSOMEkw, IFkw,
      IMPLICITkw, IMPORTkw, LAZYkw, MATCHkw, NEWkw, NULLkw, OBJECTkw,
      OVERRIDEkw, PACKAGEkw, PRIVATEkw, PROTECTEDkw, RETURNkw, REQUIRESkw,
      SEALEDkw, SUPERkw, THISkw, THROWkw, TRAITkw, TRUEkw, TRYkw, TYPEkw,
      VALkw, VARkw, WITHkw, WHILEkw, YIELDkw
    ) ++ Set[TermName](
      DOTkw, USCOREkw, COLONkw, EQUALSkw, ARROWkw, LARROWkw, SUBTYPEkw,
      VIEWBOUNDkw, SUPERTYPEkw, HASHkw, ATkw
    )

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
    private def bitmapName(n: Int, suffix: String): TermName =
      newTermName(BITMAP_PREFIX + suffix + n)

    /** The name of bitmaps for initialized (public or protected) lazy vals. */
    def bitmapName(n: Int): TermName = bitmapName(n, "")

    /** The name of bitmaps for initialized transient lazy vals. */
    def bitmapNameForTransient(n: Int): TermName = bitmapName(n, "trans$")

    /** The name of bitmaps for initialized private lazy vals. */
    def bitmapNameForPrivate(n: Int): TermName = bitmapName(n, "priv$")

    /** The name of bitmaps for checkinit values */
    def bitmapNameForCheckinit(n: Int): TermName = bitmapName(n, "init$")

    /** The name of bitmaps for checkinit values that have transient flag*/
    def bitmapNameForCheckinitTransient(n: Int): TermName = bitmapName(n, "inittrans$")

    /** Base strings from which synthetic names are derived. */
    val BITMAP_PREFIX               = "bitmap$"
    val CHECK_IF_REFUTABLE_STRING   = "check$ifrefutable$"
    val DEFAULT_GETTER_STRING       = "$default$"
    val DO_WHILE_PREFIX             = "doWhile$"
    val EQEQ_LOCAL_VAR              = "eqEqTemp$"
    val EVIDENCE_PARAM_PREFIX       = "evidence$"
    val EXCEPTION_RESULT_PREFIX     = "exceptionResult"
    val INTERPRETER_IMPORT_WRAPPER  = "$iw"
    val INTERPRETER_LINE_PREFIX     = "line"
    val INTERPRETER_SYNTHVAR_PREFIX = "synthvar$"
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
      tpnme.Byte    -> "System.Byte",
      tpnme.Char    -> "System.Char",
      tpnme.Short   -> "System.Int16",
      tpnme.Int     -> "System.Int32",
      tpnme.Long    -> "System.Int64",
      tpnme.Float   -> "System.Single",
      tpnme.Double  -> "System.Double"
    )
  }

  private class J2SENames extends JavaNames {
    final val BeanProperty: TypeName        = "scala.reflect.BeanProperty"
    final val BooleanBeanProperty: TypeName = "scala.reflect.BooleanBeanProperty"
    final val Code: TypeName                = "scala.reflect.Code"
    final val JavaSerializable: TypeName    = "java.io.Serializable"
  }

  lazy val sn: SymbolNames =
    if (forMSIL) new MSILNames
    else new J2SENames
}
