/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package symtab

import scala.collection.{ mutable, immutable }

trait StdNames extends reflect.generic.StdNames with NameManglers {
  self: SymbolTable =>

  object nme extends StandardNames with NameMangling {

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

    // Scala keywords
    val ABSTRACTkw = newTermName("abstract")
    val CASEkw = newTermName("case")
    val CLASSkw = newTermName("class")
    val CATCHkw = newTermName("catch")
    val DEFkw = newTermName("def")
    val DOkw = newTermName("do")
    val ELSEkw = newTermName("else")
    val EXTENDSkw = newTermName("extends")
    val FALSEkw = newTermName("false")
    val FINALkw = newTermName("final")
    val FINALLYkw = newTermName("finally")
    val FORkw = newTermName("for")
    val FORSOMEkw = newTermName("forSome")
    val IFkw = newTermName("if")
    val IMPLICITkw = newTermName("implicit")
    val IMPORTkw = newTermName("import")
    val LAZYkw = newTermName("lazy")
    val MATCHkw = newTermName("match")
    val NEWkw = newTermName("new")
    val NULLkw = newTermName("null")
    val OBJECTkw = newTermName("object")
    val OVERRIDEkw = newTermName("override")
    val PACKAGEkw = newTermName("package")
    val PRIVATEkw = newTermName("private")
    val PROTECTEDkw = newTermName("protected")
    val RETURNkw = newTermName("return")
    val REQUIRESkw = newTermName("requires")
    val SEALEDkw = newTermName("sealed")
    val SUPERkw = newTermName("super")
    val THISkw = newTermName("this")
    val THROWkw = newTermName("throw")
    val TRAITkw = newTermName("trait")
    val TRUEkw = newTermName("true")
    val TRYkw = newTermName("try")
    val TYPEkw = newTermName("type")
    val VALkw = newTermName("val")
    val VARkw = newTermName("var")
    val WITHkw = newTermName("with")
    val WHILEkw = newTermName("while")
    val YIELDkw = newTermName("yield")
    val DOTkw = newTermName(".")
    val USCOREkw = newTermName("_")
    val COLONkw = newTermName(":")
    val EQUALSkw = newTermName("=")
    val ARROWkw = newTermName("=>")
    val LARROWkw = newTermName("<-")
    val SUBTYPEkw = newTermName("<:")
    val VIEWBOUNDkw = newTermName("<%")
    val SUPERTYPEkw = newTermName(">:")
    val HASHkw = newTermName("#")
    val ATkw = newTermName("@")

    val INTERPRETER_IMPORT_WRAPPER    = "$iw"
    val INTERPRETER_LINE_PREFIX       = "line"
    val INTERPRETER_SYNTHVAR_PREFIX   = "synthvar$"
    val INTERPRETER_VAR_PREFIX        = "res"
    val INTERPRETER_WRAPPER_SUFFIX    = "$object"

    private def bitmapName(n: Int, suffix: String): Name =
      newTermName(BITMAP_PREFIX + suffix + n)

    /** The name of bitmaps for initialized (public or protected) lazy vals. */
    def bitmapName(n: Int): Name = bitmapName(n, "")

    /** The name of bitmaps for initialized transient lazy vals. */
    def bitmapNameForTransitive(n: Int): Name = bitmapName(n, "trans$")

    /** The name of bitmaps for initialized private lazy vals. */
    def bitmapNameForPrivate(n: Int): Name = bitmapName(n, "priv$")

    /** Base strings from which synthetic names are derived. */
    val BITMAP_PREFIX             = "bitmap$"
    val CHECK_IF_REFUTABLE_STRING = "check$ifrefutable$"
    val DEFAULT_GETTER_STRING     = "$default$"
    val DO_WHILE_PREFIX           = "doWhile$"
    val EQEQ_LOCAL_VAR            = "eqEqTemp$"
    val EVIDENCE_PARAM_PREFIX     = "evidence$"
    val EXCEPTION_RESULT_PREFIX   = "exceptionResult"
    val WHILE_PREFIX              = "while$"

    /** Internal names */
    val ANYNAME                        = newTermName("<anyname>")
    val EQUALS_PATTERN_NAME            = newTermName("<equals>")
    val ERROR                          = newTermName("<error>")
    val NOSYMBOL                       = newTermName("<none>")

    /** TYPE names. */
    val BYNAME_PARAM_CLASS_NAME        = newTypeName("<byname>")
    val JAVA_REPEATED_PARAM_CLASS_NAME = newTypeName("<repeated...>")
    val LOCALCHILD                     = newTypeName("<local child>")
    val REPEATED_PARAM_CLASS_NAME      = newTypeName("<repeated>")
    val WILDCARD_STAR                  = newTypeName("_*")

    val CONSTRUCTOR         = newTermName("<init>")
    val INITIALIZER         = newTermName("<init>")
    val INLINED_INITIALIZER = newTermName("$init$")
    val MIXIN_CONSTRUCTOR   = newTermName("$init$")

    val OUTER           = newTermName("$outer")
    val OUTER_LOCAL     = newTermName("$outer ")
    val SELF            = newTermName("$this")
    val THIS            = newTermName("_$this")
    val FAKE_LOCAL_THIS = newTermName("this$")

    val MODULE_INSTANCE_FIELD = newTermName("MODULE$")
    val SPECIALIZED_INSTANCE  = newTermName("specInstance$")

    val TYPE_         = newTermName("TYPE")
    val WILDCARD      = newTermName("_")
    val STAR          = newTermName("*")

    val MINUS = encode("-")
    val PLUS = encode("+")
    val PLUSPLUS = encode("++")
    val TILDE = encode("~")
    val BANG = encode("!")
    val BANGEQ = encode("!=")
    val BARBAR = encode("||")
    val AMPAMP = encode("&&")
    val COLONCOLON = encode("::")
    val PERCENT = encode("%")
    val EQL = encode("=")
    val HASHHASH = encode("##")

    val Nothing = newTermName("Nothing")
    val Null = newTermName("Null")
    val Any = newTermName("Any")
    val AnyVal = newTermName("AnyVal")
    val AnyRef = newTermName("AnyRef")
    val Array = newTermName("Array")
    val Boolean = newTermName("Boolean")
    val Byte = newTermName("Byte")
    val Char = newTermName("Char")
    val Do = newTermName("Do")
    val Double = newTermName("Double")
    val Float = newTermName("Float")
    val Function = newTermName("Function")
    val Int = newTermName("Int")
    val List = newTermName("List")
    val Long = newTermName("Long")
    val Nil = newTermName("Nil")
    val Object = newTermName("Object")
    val PartialFunction = newTermName("PartialFunction")
    val Predef = newTermName("Predef")
    val Product = newTermName("Product")
    val ScalaObject = newTermName("ScalaObject")
    val ScalaRunTime = newTermName("ScalaRunTime")
    val Seq = newTermName("Seq")
    val Serializable = newTermName("Serializable")
    val Short = newTermName("Short")
    val Singleton = newTermName("Singleton")
    val Some = newTermName("Some")
    val String = newTermName("String")
    val Symbol = newTermName("Symbol")
    val System = newTermName("System")
    val Throwable = newTermName("Throwable")
    val Tuple = newTermName("Tuple")
    val Unit = newTermName("Unit")

    val apply = newTermName("apply")
    val arrayValue = newTermName("arrayValue")
    val arraycopy = newTermName("arraycopy")
    val assert_ = newTermName("assert")
    val assume_ = newTermName("assume")
    val asInstanceOf_ = newTermName("asInstanceOf")
    val box = newTermName("box")
    val bytes = newTermName("bytes")
    val canEqual_ = newTermName("canEqual")
    val checkInitialized = newTermName("checkInitialized")
    val classOf = newTermName("classOf")
    val identity = newTermName("identity")
    val conforms = newTermName("conforms")
    val copy = newTermName("copy")
    val delayedInit = newTermName("delayedInit")
    val delayedInitArg = newTermName("delayedInit$body")
    val dottype = newTermName(".type")
    val drop = newTermName("drop")
    val elem = newTermName("elem")
    val eq = newTermName("eq")
    val equals_ = newTermName("equals")
    val inlinedEquals = newTermName("inlinedEquals")
    val error = newTermName("error")
    val ex = newTermName("ex")
    val add_ = newTermName("add")
    val false_ = newTermName("false")
    val filter = newTermName("filter")
    val finalize_ = newTermName("finalize")
    val find_ = newTermName("find")
    val flatMap = newTermName("flatMap")
    val foreach = newTermName("foreach")
    val get = newTermName("get")
    def getCause = sn.GetCause
    def getClass_ = sn.GetClass
    def getMethod_ = sn.GetMethod
    val hash_ = newTermName("hash")
    val hashCode_ = newTermName("hashCode")
    val hasNext = newTermName("hasNext")
    val head = newTermName("head")
    def invoke_ = sn.Invoke
    val isArray = newTermName("isArray")
    val isInstanceOf_ = newTermName("isInstanceOf")
    val isDefinedAt = newTermName("isDefinedAt")
    val isEmpty = newTermName("isEmpty")
    val java = newTermName("java")
    val lang = newTermName("lang")
    val length = newTermName("length")
    val lengthCompare = newTermName("lengthCompare")
    val lift_ = newTermName("lift")
    val main = newTermName("main")
    val map = newTermName("map")
    val ne = newTermName("ne")
    val newArray = newTermName("newArray")
    val next = newTermName("next")
    val notify_ = newTermName("notify")
    val notifyAll_ = newTermName("notifyAll")
    val null_ = newTermName("null")
    val ofDim = newTermName("ofDim")
    val productArity = newTermName("productArity")
    val productElement = newTermName("productElement")
    // val productElementName = newTermName("productElementName")
    val productPrefix = newTermName("productPrefix")
    val readResolve = newTermName("readResolve")
    val sameElements = newTermName("sameElements")
    val scala_ = newTermName("scala")
    val self = newTermName("self")
    val setAccessible = newTermName("setAccessible")
    val synchronized_ = newTermName("synchronized")
    val tail = newTermName("tail")
    val toArray = newTermName("toArray")
    val toList = newTermName("toList")
    val toSeq = newTermName("toSeq")
    val toString_ = newTermName("toString")
    val clone_ = newTermName("clone")
    val this_ = newTermName("this")
    val throw_ = newTermName("throw")
    val true_ = newTermName("true")
    val unapply = newTermName("unapply")
    val unapplySeq = newTermName("unapplySeq")
    val unbox = newTermName("unbox")
    val update = newTermName("update")
    val value = newTermName("value")
    val view_ = newTermName("view")
    val wait_ = newTermName("wait")
    val withFilter = newTermName("withFilter")
    val zip = newTermName("zip")
    val genericArrayOps = newTermName("genericArrayOps")

    val ZAND = encode("&&")
    val ZOR  = encode("||")
    val ADD  = encode("+")
    val SUB  = encode("-")
    val MUL  = encode("*")
    val DIV  = encode("/")
    val MOD  = encode("%")
    val EQ   = encode("==")
    val NE   = encode("!=")
    val LT   = encode("<")
    val LE   = encode("<=")
    val GT   = encode(">")
    val GE   = encode(">=")
    val OR   = encode("|")
    val XOR  = encode("^")
    val AND  = encode("&")
    val LSL  = encode("<<")
    val LSR  = encode(">>>")
    val ASR  = encode(">>")

    // unary operators
    val UNARY_~ = encode("unary_~")
    val UNARY_+ = encode("unary_+")
    val UNARY_- = encode("unary_-")
    val UNARY_! = encode("unary_!")

    // unencoded comparisons
    val EQraw = newTermName("==")
    val NEraw = newTermName("!=")
    val LEraw = newTermName("<=")
    val GEraw = newTermName(">=")
    val DOLLARraw = newTermName("$")

    // value-conversion methods
    val toByte = newTermName("toByte")
    val toShort = newTermName("toShort")
    val toChar = newTermName("toChar")
    val toInt = newTermName("toInt")
    val toLong = newTermName("toLong")
    val toFloat = newTermName("toFloat")
    val toDouble = newTermName("toDouble")

    val SourceFileATTR = newTermName("SourceFile")
    val SyntheticATTR = newTermName("Synthetic")
    val BridgeATTR = newTermName("Bridge")
    val DeprecatedATTR = newTermName("Deprecated")
    val CodeATTR = newTermName("Code")
    val ExceptionsATTR = newTermName("Exceptions")
    val ConstantValueATTR = newTermName("ConstantValue")
    val LineNumberTableATTR = newTermName("LineNumberTable")
    val LocalVariableTableATTR = newTermName("LocalVariableTable")
    val InnerClassesATTR = newTermName("InnerClasses")
    val JacoMetaATTR = newTermName("JacoMeta")
    val SignatureATTR = newTermName("Signature")
    val ScalaSignatureATTR = newTermName("ScalaSig")
    val AnnotationDefaultATTR = newTermName("AnnotationDefault")
    // Stores Java annotations with RetentionPolicy.RUNTIME
    val RuntimeAnnotationATTR = newTermName("RuntimeVisibleAnnotations")
    // Stores Java annotations with RetentionPolicy.CLASS. Currently not used (Apr 2009).
    val ClassfileAnnotationATTR = newTermName("RuntimeInvisibleAnnotations")
    // Stores Java annotations on parameters with RetentionPolicy.RUNTIME
    val RuntimeParamAnnotationATTR = newTermName("RuntimeVisibleParameterAnnotations")
    val ScalaATTR = newTermName("Scala")
  }

  abstract class SymbolNames {
    val BeanProperty        : Name
    val BooleanBeanProperty : Name
    val BoxedBoolean        : Name
    val BoxedCharacter      : Name
    val BoxedNumber         : Name
    val Class               : Name
    val Code                : Name
    val Delegate            : Name
    val ForName             : Name
    val GetCause            : Name
    val GetClass            : Name
    val GetMethod           : Name
    val IOOBException       : Name // IndexOutOfBoundsException
    val InvTargetException  : Name // InvocationTargetException
    val Invoke              : Name
    val JavaLang            : Name
    val MethodAsObject      : Name
    val NLRControl          : Name = newTermName("scala.runtime.NonLocalReturnControl")
    val NPException         : Name // NullPointerException
    val Object              : Name
    val JavaSerializable    : Name
    val String              : Name
    val Throwable           : Name
    val ValueType           : Name

    val Boxed: immutable.Map[Name, Name]
  }

  private abstract class JavaNames extends SymbolNames {
    final val BoxedBoolean       = newTermName("java.lang.Boolean")
    final val BoxedByte          = newTermName("java.lang.Byte")
    final val BoxedCharacter     = newTermName("java.lang.Character")
    final val BoxedDouble        = newTermName("java.lang.Double")
    final val BoxedFloat         = newTermName("java.lang.Float")
    final val BoxedInteger       = newTermName("java.lang.Integer")
    final val BoxedLong          = newTermName("java.lang.Long")
    final val BoxedNumber        = newTermName("java.lang.Number")
    final val BoxedShort         = newTermName("java.lang.Short")
    final val Class              = newTermName("java.lang.Class")
    final val Delegate           = nme.NOSYMBOL
    final val ForName            = newTermName("forName")
    final val GetCause           = newTermName("getCause")
    final val GetClass           = newTermName("getClass")
    final val GetMethod          = newTermName("getMethod")
    final val IOOBException      = newTermName("java.lang.IndexOutOfBoundsException")
    final val InvTargetException = newTermName("java.lang.reflect.InvocationTargetException")
    final val Invoke             = newTermName("invoke")
    final val JavaLang           = newTermName("java.lang")
    final val MethodAsObject     = newTermName("java.lang.reflect.Method")
    final val NPException        = newTermName("java.lang.NullPointerException")
    final val Object             = newTermName("java.lang.Object")
    final val String             = newTermName("java.lang.String")
    final val Throwable          = newTermName("java.lang.Throwable")
    final val ValueType          = nme.NOSYMBOL

    val Boxed = immutable.Map[Name, Name](
      nme.Boolean -> BoxedBoolean,
      nme.Byte    -> BoxedByte,
      nme.Char    -> BoxedCharacter,
      nme.Short   -> BoxedShort,
      nme.Int     -> BoxedInteger,
      nme.Long    -> BoxedLong,
      nme.Float   -> BoxedFloat,
      nme.Double  -> BoxedDouble
    )
  }

  private class MSILNames extends SymbolNames {
    final val BeanProperty        = nme.NOSYMBOL
    final val BooleanBeanProperty = nme.NOSYMBOL
    final val BoxedBoolean        = newTermName("System.IConvertible")
    final val BoxedCharacter      = newTermName("System.IConvertible")
    final val BoxedNumber         = newTermName("System.IConvertible")
    final val Class               = newTermName("System.Type")
    final val Code                = nme.NOSYMBOL
    final val Delegate            = newTermName("System.MulticastDelegate")
    final val ForName             = newTermName("GetType")
    final val GetCause            = newTermName("InnerException") /* System.Reflection.TargetInvocationException.InnerException */
    final val GetClass            = newTermName("GetType")
    final val GetMethod           = newTermName("GetMethod")
    final val IOOBException       = newTermName("System.IndexOutOfRangeException")
    final val InvTargetException  = newTermName("System.Reflection.TargetInvocationException")
    final val Invoke              = newTermName("Invoke")
    final val JavaLang            = newTermName("System")
    final val MethodAsObject      = newTermName("System.Reflection.MethodInfo")
    final val NPException         = newTermName("System.NullReferenceException")
    final val Object              = newTermName("System.Object")
    final val JavaSerializable    = nme.NOSYMBOL
    final val String              = newTermName("System.String")
    final val Throwable           = newTermName("System.Exception")
    final val ValueType           = newTermName("System.ValueType")

    val Boxed = immutable.Map[Name, Name](
      nme.Boolean -> newTermName("System.Boolean"),
      nme.Byte    -> newTermName("System.Byte"),
      nme.Char    -> newTermName("System.Char"),
      nme.Short   -> newTermName("System.Int16"),
      nme.Int     -> newTermName("System.Int32"),
      nme.Long    -> newTermName("System.Int64"),
      nme.Float   -> newTermName("System.Single"),
      nme.Double  -> newTermName("System.Double")
    )
  }

  private class J2SENames extends JavaNames {
    final val BeanProperty        = newTermName("scala.reflect.BeanProperty")
    final val BooleanBeanProperty = newTermName("scala.reflect.BooleanBeanProperty")
    final val Code                = newTermName("scala.reflect.Code")
    final val JavaSerializable    = newTermName("java.io.Serializable")
  }

  lazy val sn: SymbolNames =
    if (forMSIL) new MSILNames
    else new J2SENames
}
