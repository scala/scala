/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package symtab

import scala.reflect.NameTransformer

trait StdNames extends reflect.generic.StdNames { self: SymbolTable =>

  object nme extends StandardNames {

    // Scala keywords; enter them first to minimize scanner.maxKey
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
    val MIXINkw = newTermName("mixin")
    val NEWkw = newTermName("new")
    val NULLkw = newTermName("null")
    val OBJECTkw = newTermName("object")
    val OUTER = newTermName("$outer")
    val OUTER_LOCAL = newTermName("$outer ")
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
    val TYPE_ = newTermName("TYPE")
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

    val LOCALDUMMY_PREFIX_STRING = "<local "
    val SUPER_PREFIX_STRING = "super$"
    val TRAIT_SETTER_SEPARATOR_STRING = "$_setter_$"
    val TUPLE_FIELD_PREFIX_STRING = "_"
    val CHECK_IF_REFUTABLE_STRING = "check$ifrefutable$"

    val INTERPRETER_WRAPPER_SUFFIX = "$object"
    val INTERPRETER_LINE_PREFIX = "line"
    val INTERPRETER_VAR_PREFIX = "res"
    val INTERPRETER_IMPORT_WRAPPER = "$iw"
    val INTERPRETER_SYNTHVAR_PREFIX = "synthvar$"
    val EVIDENCE_PARAM_PREFIX = "evidence$"

    def LOCAL(clazz: Symbol) = newTermName(LOCALDUMMY_PREFIX_STRING + clazz.name+">")
    def TUPLE_FIELD(index: Int) = newTermName(TUPLE_FIELD_PREFIX_STRING + index)

    val LOCAL_SUFFIX = newTermName(LOCAL_SUFFIX_STRING)
    val SETTER_SUFFIX = encode("_=")
    val IMPL_CLASS_SUFFIX = newTermName("$class")
    val MODULE_SUFFIX = newTermName("$module")
    val LOCALDUMMY_PREFIX = newTermName(LOCALDUMMY_PREFIX_STRING)
    val SELECTOR_DUMMY = newTermName("<unapply-selector>")

    val MODULE_INSTANCE_FIELD = newTermName("MODULE$")

    def isLocalName(name: Name) = name.endsWith(LOCAL_SUFFIX)
    def isSetterName(name: Name) = name.endsWith(SETTER_SUFFIX)
    def isLocalDummyName(name: Name) = name.startsWith(LOCALDUMMY_PREFIX)
    def isTraitSetterName(name: Name) = isSetterName(name) && name.pos(TRAIT_SETTER_SEPARATOR_STRING) < name.length
    def isOpAssignmentName(name: Name) =
      name(name.length - 1) == '=' &&
      isOperatorCharacter(name(0)) &&
      name(0) != '=' && name != NEraw && name != LEraw && name != GEraw

    def isOperatorCharacter(c: Char) = c match {
      case '~' | '!' | '@' | '#' | '%' |
           '^' | '*' | '+' | '-' | '<' |
           '>' | '?' | ':' | '=' | '&' |
           '|' | '\\'| '/' => true
      case _ =>
        val chtp = Character.getType(c)
        chtp == Character.MATH_SYMBOL.toInt || chtp == Character.OTHER_SYMBOL.toInt
      }

    /** The expanded setter name of `name' relative to this class `base`
     */
    def expandedSetterName(name: Name, base: Symbol): Name =
      expandedName(name, base, separator = TRAIT_SETTER_SEPARATOR_STRING)

    /** If `name' is an expandedName name, the original name.
     *  Otherwise `name' itself.
     */
    def originalName(name: Name): Name = {
      var i = name.length
      while (i >= 2 && !(name(i - 1) == '$' && name(i - 2) == '$')) i -= 1
      if (i >= 2) {
        while (i >= 3 && name(i - 3) == '$') i -= 1
        name.subName(i, name.length)
      } else name
    }

    def localToGetter(name: Name): Name = {
      assert(isLocalName(name))//debug
      name.subName(0, name.length - LOCAL_SUFFIX.length)
    }

    def getterToLocal(name: Name): Name = {
      newTermName(name.toString() + LOCAL_SUFFIX)
    }

    def getterToSetter(name: Name): Name = {
      newTermName(name.toString() + SETTER_SUFFIX)
    }

    def setterToGetter(name: Name): Name = {
      val p = name.pos(TRAIT_SETTER_SEPARATOR_STRING)
      if (p < name.length)
        setterToGetter(name.subName(p + TRAIT_SETTER_SEPARATOR_STRING.length, name.length))
      else
        name.subName(0, name.length - SETTER_SUFFIX.length)
    }

    def getterName(name: Name): Name =
      if (isLocalName(name)) localToGetter(name) else name;

    def isImplClassName(name: Name): Boolean =
      name endsWith IMPL_CLASS_SUFFIX;

    def implClassName(name: Name): Name =
      newTypeName(name.toString() + IMPL_CLASS_SUFFIX)

    def interfaceName(implname: Name): Name =
      implname.subName(0, implname.length - IMPL_CLASS_SUFFIX.length)

    def moduleVarName(name: Name): Name =
      newTermName(name.toString() + MODULE_SUFFIX)

    def superName(name: Name) = newTermName("super$" + name)

    val PROTECTED_PREFIX = "protected$"
    def isProtectedAccessor(name: Name) = name.startsWith(PROTECTED_PREFIX)

    /** The name of an accessor for protected symbols. */
    def protName(name: Name): Name = newTermName(PROTECTED_PREFIX + name)

    /** The name of a setter for protected symbols. Used for inherited Java fields. */
    def protSetterName(name: Name): Name = newTermName(PROTECTED_PREFIX + "set" + name)

    /** The name of bitmaps for initialized lazy vals. */
    def bitmapName(n: Int): Name = newTermName("bitmap$" + n)

    val ERROR = newTermName("<error>")
    val LOCALCHILD = newTypeName("<local child>")

    val NOSYMBOL = newTermName("<none>")
    val ANYNAME = newTermName("<anyname>")
    val WILDCARD = newTermName("_")
    val WILDCARD_STAR = newTermName("_*")

    val STAR = newTermName("*")
    val REPEATED_PARAM_CLASS_NAME = newTermName("<repeated>")
    val JAVA_REPEATED_PARAM_CLASS_NAME = newTermName("<repeated...>")
    val BYNAME_PARAM_CLASS_NAME = newTermName("<byname>")
    val EQUALS_PATTERN_NAME = newTermName("<equals>")
    val SELF = newTermName("$this")
    val THIS = newTermName("_$this")

    val CONSTRUCTOR = newTermName("<init>")
    val MIXIN_CONSTRUCTOR = newTermName("$init$")
    val INITIALIZER = newTermName("<init>")
    val INLINED_INITIALIZER = newTermName("$init$")

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
    val USCOREEQL = encode("_=")

    val Nothing = newTermName("Nothing")
    val Null = newTermName("Null")
    val Any = newTermName("Any")
    val AnyVal = newTermName("AnyVal")
    val AnyRef = newTermName("AnyRef")
    val Array = newTermName("Array")
    val Boolean = newTermName("Boolean")
    val Byte = newTermName("Byte")
    val Catch = newTermName("Catch")
    val Char = newTermName("Char")
    val Do = newTermName("Do")
    val Double = newTermName("Double")
    val Finally = newTermName("Finally")
    val Float = newTermName("Float")
    val Function = newTermName("Function")
    val Function1 = newTermName("Function1")
    val Int = newTermName("Int")
    val List = newTermName("List")
    val Long = newTermName("Long")
    val Nil = newTermName("Nil")
    val Object = newTermName("Object")
    val PartialFunction = newTermName("PartialFunction")
    val Predef = newTermName("Predef")
    val Product = newTermName("Product")
    def Product_(i:Int) = newTermName("_" + i)
    val ScalaObject = newTermName("ScalaObject")
    val ScalaRunTime = newTermName("ScalaRunTime")
    val Seq = newTermName("Seq")
    val Short = newTermName("Short")
    val Singleton = newTermName("Singleton")
    val Some = newTermName("Some")
    val String = newTermName("String")
    val Symbol = newTermName("Symbol")
    val System = newTermName("System")
    val Throwable = newTermName("Throwable")
    val Try = newTermName("Try")
    val Tuple = newTermName("Tuple")
    val Tuple2 = newTermName("Tuple2")
    val Unit = newTermName("Unit")

    val apply = newTermName("apply")
    val arrayValue = newTermName("arrayValue")
    val arraycopy = newTermName("arraycopy")
    val assert_ = newTermName("assert")
    val assume_ = newTermName("assume")
    val asInstanceOf_ = newTermName("asInstanceOf")
    val box = newTermName("box")
    val canEqual_ = newTermName("canEqual")
    val checkInitialized = newTermName("checkInitialized")
    val classOf = newTermName("classOf")
    val identity = newTermName("identity")
    val conforms = newTermName("conforms")
    val copy = newTermName("copy")
    val dottype = newTermName(".type")
    val drop = newTermName("drop")
    val elem = newTermName("elem")
    val eq = newTermName("eq")
    val equals_ = newTermName("equals")
    val _equals = newTermName("_equals")
    val inlinedEquals = newTermName("inlinedEquals")
    val error = newTermName("error")
    val ex = newTermName("ex")
    val add_ = newTermName("add")
    val false_ = newTermName("false")
    val filter = newTermName("filter")
    val finalize_ = newTermName("finalize")
    val find_ = newTermName("find")
    val flatMap = newTermName("flatMap")
    val forName = newTermName(if (forMSIL) "GetType" else "forName")
    val foreach = newTermName("foreach")
    val get = newTermName("get")
    val getCause = newTermName("getCause")
    val getClass_ = newTermName("getClass")
    val getMethod_ = newTermName("getMethod")
    val hashCode_ = newTermName("hashCode")
    val hasNext = newTermName("hasNext")
    val head = newTermName("head")
    val invoke_ = newTermName("invoke")
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
    val Mutable = newTypeName("Mutable")
    val ne = newTermName("ne")
    val newArray = newTermName("newArray")
    val next = newTermName("next")
    val notify_ = newTermName("notify")
    val notifyAll_ = newTermName("notifyAll")
    val null_ = newTermName("null")
    val ofDim = newTermName("ofDim")
    val print = newTermName("print")
    val productArity = newTermName("productArity")
    val productElement = newTermName("productElement")
    val productPrefix = newTermName("productPrefix")
    val readResolve = newTermName("readResolve")
    val sameElements = newTermName("sameElements")
    val scala_ = newTermName("scala")
    val self = newTermName("self")
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

  def encode(str: String): Name = newTermName(NameTransformer.encode(str))

  abstract class SymbolNames {
    val JavaLang     : Name
    val Object       : Name
    val Class        : Name
    val String       : Name
    val Throwable    : Name
    val NPException  : Name // NullPointerException
    val NLRException : Name = newTermName("scala.runtime.NonLocalReturnException")
    val ValueType    : Name
    val Serializable : Name
    val BeanProperty : Name
    val BooleanBeanProperty: Name
    val Delegate     : Name
    val IOOBException: Name // IndexOutOfBoundsException
    val Code         : Name
    val BoxedNumber  : Name
    val BoxedCharacter : Name
    val BoxedBoolean : Name
    val MethodAsObject : Name

    import scala.collection.mutable.HashMap
    val Boxed = new HashMap[Name, Name]
  }

  private abstract class JavaNames extends SymbolNames {
    final val JavaLang      = newTermName("java.lang")
    final val Object        = newTermName("java.lang.Object")
    final val Class         = newTermName("java.lang.Class")
    final val String        = newTermName("java.lang.String")
    final val Throwable     = newTermName("java.lang.Throwable")
    final val NPException   = newTermName("java.lang.NullPointerException")
    final val ValueType     = nme.NOSYMBOL
    final val Delegate      = nme.NOSYMBOL
    final val IOOBException = newTermName("java.lang.IndexOutOfBoundsException")
    final val BoxedNumber   = newTermName("java.lang.Number")
    final val BoxedCharacter = newTermName("java.lang.Character")
    final val BoxedBoolean  = newTermName("java.lang.Boolean")
    final val BoxedByte     = newTermName("java.lang.Byte")
    final val BoxedShort    = newTermName("java.lang.Short")
    final val BoxedInteger  = newTermName("java.lang.Integer")
    final val BoxedLong     = newTermName("java.lang.Long")
    final val BoxedFloat    = newTermName("java.lang.Float")
    final val BoxedDouble   = newTermName("java.lang.Double")

    final val MethodAsObject = newTermName("java.lang.reflect.Method")

    Boxed += (nme.Boolean -> BoxedBoolean)
    Boxed += (nme.Byte    -> BoxedByte)
    Boxed += (nme.Char    -> BoxedCharacter)
    Boxed += (nme.Short   -> BoxedShort)
    Boxed += (nme.Int     -> BoxedInteger)
    Boxed += (nme.Long    -> BoxedLong)
    Boxed += (nme.Float   -> BoxedFloat)
    Boxed += (nme.Double  -> BoxedDouble)
  }

  private class MSILNames extends SymbolNames {
    final val JavaLang      = newTermName("System")
    final val Object        = newTermName("System.Object")
    final val Class         = newTermName("System.Type")
    final val String        = newTermName("System.String")
    final val Throwable     = newTermName("System.Exception")
    final val NPException   = newTermName("System.NullReferenceException")
    final val ValueType     = newTermName("System.ValueType")
    final val Serializable  = nme.NOSYMBOL
    final val BeanProperty  = nme.NOSYMBOL
    final val BooleanBeanProperty = nme.NOSYMBOL
    final val Delegate      = newTermName("System.MulticastDelegate")
    final val IOOBException = newTermName("System.IndexOutOfRangeException")
    final val Code          = nme.NOSYMBOL
    final val BoxedNumber   = newTermName("System.IConvertible")
    final val BoxedCharacter = newTermName("System.IConvertible")
    final val BoxedBoolean = newTermName("System.IConvertible")
    final val MethodAsObject = nme.NOSYMBOL // TODO: is there something like Method in MSIL?

    Boxed += (nme.Boolean -> newTermName("System.Boolean"))
    Boxed += (nme.Byte    -> newTermName("System.Byte"))
    Boxed += (nme.Char    -> newTermName("System.Char"))
    Boxed += (nme.Short   -> newTermName("System.Int16"))
    Boxed += (nme.Int     -> newTermName("System.Int32"))
    Boxed += (nme.Long    -> newTermName("System.Int64"))
    Boxed += (nme.Float   -> newTermName("System.Single"))
    Boxed += (nme.Double  -> newTermName("System.Double"))
  }

  private class J2SENames extends JavaNames {
    final val Serializable  = newTermName("java.io.Serializable")
    final val BeanProperty  = newTermName("scala.reflect.BeanProperty")
    final val BooleanBeanProperty  = newTermName("scala.reflect.BooleanBeanProperty")
    final val Code          = newTermName("scala.reflect.Code")
  }

  lazy val sn: SymbolNames =
    if (forMSIL) new MSILNames
    else new J2SENames
}
