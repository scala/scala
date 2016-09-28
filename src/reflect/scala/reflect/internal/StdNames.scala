/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
package internal

import scala.language.implicitConversions

import java.security.MessageDigest
import Chars.isOperatorPart
import scala.annotation.switch
import scala.collection.immutable
import scala.io.Codec

trait StdNames {
  self: SymbolTable =>

  def encode(str: String): TermName = newTermNameCached(NameTransformer.encode(str))

  /** Tensions: would like the keywords to be the very first names entered into the names
   *  storage so their ids count from 0, which simplifies the parser. Switched to abstract
   *  classes to avoid all the indirection which is generated with implementation-containing
   *  traits. Since all these classes use eager vals, that means the constructor with the
   *  keywords must run first. If it's the top in the superclass chain, then CommonNames
   *  must inherit from it, which means TypeNames would inherit keywords as well.
   *
   *  Solution: Keywords extends CommonNames and uses early defs to beat the
   *  CommonNames constructor out of the starting gate.  This is its builder.
   */
  private class KeywordSetBuilder {
    private var kws: Set[TermName] = Set()
    def apply(s: String): TermName = {
      val result = newTermNameCached(s)
      kws = kws + result
      result
    }
    def result: Set[TermName] = try kws finally kws = null
  }

  private[reflect] def compactifyName(orig: String): String = compactify(orig)
  private final object compactify extends (String => String) {
    val md5 = MessageDigest.getInstance("MD5")

    /**
     * COMPACTIFY
     *
     * The hashed name has the form (prefix + marker + md5 + marker + suffix), where
     *   - prefix/suffix.length = MaxNameLength / 4
     *   - md5.length = 32
     *
     * We obtain the formula:
     *
     *   FileNameLength = 2*(MaxNameLength / 4) + 2.marker.length + 32 + suffixLength
     *
     * (+suffixLength for ".class" and potential module class suffix that is added *after* this transform).
     *
     * MaxNameLength can therefore be computed as follows:
     */
    val marker = "$$$$"
    val maxSuffixLength = "$.class".length + 1 // potential module class suffix and file extension
    val MaxNameLength = math.min(
      settings.maxClassfileName.value - maxSuffixLength,
      2 * (settings.maxClassfileName.value - maxSuffixLength - 2*marker.length - 32)
    )
    def toMD5(s: String, edge: Int): String = {
      val prefix = s take edge
      val suffix = s takeRight edge

      val cs = s.toArray
      val bytes = Codec toUTF8 cs
      md5 update bytes
      val md5chars = (md5.digest() map (b => (b & 0xFF).toHexString)).mkString

      prefix + marker + md5chars + marker + suffix
    }
    def apply(s: String): String = (
      if (s.length <= MaxNameLength) s
      else toMD5(s, MaxNameLength / 4)
    )
  }

  abstract class CommonNames extends NamesApi {
    type NameType >: Null <: Name
    // Masking some implicits so as to allow our targeted => NameType.
    protected val stringToTermName = null
    protected val stringToTypeName = null
    protected implicit def createNameType(name: String): NameType

    def flattenedName(segments: Name*): NameType =
      compactify(segments mkString NAME_JOIN_STRING)

    // TODO: what is the purpose of all this duplication!?!?!
    // I made these constants because we cannot change them without bumping our major version anyway.
    final val NAME_JOIN_STRING                 = NameTransformer.NAME_JOIN_STRING
    final val MODULE_SUFFIX_STRING             = NameTransformer.MODULE_SUFFIX_STRING
    final val MODULE_VAR_SUFFIX_STRING         = NameTransformer.MODULE_VAR_SUFFIX_STRING
    final val LOCAL_SUFFIX_STRING              = NameTransformer.LOCAL_SUFFIX_STRING
    final val LAZY_LOCAL_SUFFIX_STRING         = NameTransformer.LAZY_LOCAL_SUFFIX_STRING
    final val TRAIT_SETTER_SEPARATOR_STRING    = NameTransformer.TRAIT_SETTER_SEPARATOR_STRING
    final val SINGLETON_SUFFIX                 = ".type"

    val ANON_CLASS_NAME: NameType              = "$anon"
    val DELAMBDAFY_LAMBDA_CLASS_NAME: NameType = "$lambda"
    val ANON_FUN_NAME: NameType                = "$anonfun"
    val EMPTY: NameType                        = ""
    val EMPTY_PACKAGE_NAME: NameType           = "<empty>"
    val IMPORT: NameType                       = "<import>"
    val MODULE_SUFFIX_NAME: NameType           = MODULE_SUFFIX_STRING
    val MODULE_VAR_SUFFIX: NameType            = MODULE_VAR_SUFFIX_STRING
    val PACKAGE: NameType                      = "package"
    val ROOT: NameType                         = "<root>"
    val SPECIALIZED_SUFFIX: NameType           = "$sp"
    val CASE_ACCESSOR: NameType                = "$access"

    val NESTED_IN: String                      = "$nestedIn"
    val NESTED_IN_ANON_CLASS: String           = NESTED_IN + ANON_CLASS_NAME.toString.replace("$", "")
    val NESTED_IN_ANON_FUN: String             = NESTED_IN + ANON_FUN_NAME.toString.replace("$", "")
    val NESTED_IN_LAMBDA: String               = NESTED_IN + DELAMBDAFY_LAMBDA_CLASS_NAME.toString.replace("$", "")

    /**
     * Ensures that name mangling does not accidentally make a class respond `true` to any of
     * isAnonymousClass, isAnonymousFunction, isDelambdafyFunction, e.g. by introducing "$anon".
     */
    def ensureNonAnon(name: String) = {
      name
        .replace(nme.ANON_CLASS_NAME.toString, NESTED_IN_ANON_CLASS)
        .replace(nme.ANON_FUN_NAME.toString, NESTED_IN_ANON_FUN)
        .replace(nme.DELAMBDAFY_LAMBDA_CLASS_NAME.toString, NESTED_IN_LAMBDA)
    }


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

    // some types whose companions we utilize
    final val AnyRef: NameType        = "AnyRef"
    final val Array: NameType         = "Array"
    final val List: NameType          = "List"
    final val Option: NameType        = "Option"
    final val Seq: NameType           = "Seq"
    final val Symbol: NameType        = "Symbol"
    final val WeakTypeTag: NameType   = "WeakTypeTag"
    final val TypeTag : NameType      = "TypeTag"
    final val Expr: NameType          = "Expr"
    final val String: NameType        = "String"
    final val StringContext: NameType = "StringContext"

    // fictions we use as both types and terms
    final val ERROR: NameType    = "<error>"
    final val NO_NAME: NameType  = "<none>"  // formerly NOSYMBOL
    final val WILDCARD: NameType = "_"
  }

  /** This should be the first trait in the linearization. */
  // abstract class Keywords extends CommonNames {
  abstract class Keywords extends {
    private val kw = new KeywordSetBuilder

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
    final val MACROkw: TermName     = kw("macro")
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
    final val THENkw: TermName      = kw("then")
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

    final val keywords = kw.result
  } with CommonNames {
    final val javaKeywords = new JavaKeywords()
  }

  abstract class TypeNames extends Keywords with TypeNamesApi {
    override type NameType = TypeName

    protected implicit def createNameType(name: String): TypeName = newTypeNameCached(name)

    final val BYNAME_PARAM_CLASS_NAME: NameType        = "<byname>"
    final val JAVA_REPEATED_PARAM_CLASS_NAME: NameType = "<repeated...>"
    final val LOCAL_CHILD: NameType                    = "<local child>"
    final val REFINE_CLASS_NAME: NameType              = "<refinement>"
    final val REPEATED_PARAM_CLASS_NAME: NameType      = "<repeated>"
    final val WILDCARD_STAR: NameType                  = "_*"
    final val REIFY_TREECREATOR_PREFIX: NameType       = "$treecreator"
    final val REIFY_TYPECREATOR_PREFIX: NameType       = "$typecreator"
    final val MACRO_BUNDLE_SUFFIX: NameType            = "$Bundle"

    final val Any: NameType             = "Any"
    final val AnyVal: NameType          = "AnyVal"
    final val App: NameType             = "App"
    final val FlagSet: NameType         = "FlagSet"
    final val Mirror: NameType          = "Mirror"
    final val Modifiers: NameType       = "Modifiers"
    final val Nothing: NameType         = "Nothing"
    final val Null: NameType            = "Null"
    final val Object: NameType          = "Object"
    final val PrefixType: NameType      = "PrefixType"
    final val Product: NameType         = "Product"
    final val Serializable: NameType    = "Serializable"
    final val Singleton: NameType       = "Singleton"
    final val Throwable: NameType       = "Throwable"
    final val unchecked: NameType       = "unchecked"

    final val api: NameType                 = "api"
    final val Annotation: NameType          = "Annotation"
    final val CaseDef: NameType             = "CaseDef"
    final val ClassfileAnnotation: NameType = "ClassfileAnnotation"
    final val ClassManifest: NameType       = "ClassManifest"
    final val Enum: NameType                = "Enum"
    final val Group: NameType               = "Group"
    final val implicitNotFound: NameType    = "implicitNotFound"
    final val Liftable: NameType            = "Liftable"
    final val Unliftable: NameType          = "Unliftable"
    final val Name: NameType                = "Name"
    final val Tree: NameType                = "Tree"
    final val Text: NameType                = "Text"
    final val TermName: NameType            = "TermName"
    final val Type : NameType               = "Type"
    final val TypeName: NameType            = "TypeName"
    final val TypeDef: NameType             = "TypeDef"
    final val Quasiquote: NameType          = "Quasiquote"

    // quasiquote-specific names
    final val QUASIQUOTE_FUNCTION: NameType     = "$quasiquote$function$"
    final val QUASIQUOTE_MODS: NameType         = "$quasiquote$mods$"
    final val QUASIQUOTE_TUPLE: NameType        = "$quasiquote$tuple$"

    // Annotation simple names, used in Namer
    final val BeanPropertyAnnot: NameType = "BeanProperty"
    final val BooleanBeanPropertyAnnot: NameType = "BooleanBeanProperty"
    final val bridgeAnnot: NameType = "bridge"

    // Classfile Attributes
    final val AnnotationDefaultATTR: NameType      = "AnnotationDefault"
    final val BridgeATTR: NameType                 = "Bridge"
    final val CodeATTR: NameType                   = "Code"
    final val ConstantValueATTR: NameType          = "ConstantValue"
    final val DeprecatedATTR: NameType             = "Deprecated"
    final val ExceptionsATTR: NameType             = "Exceptions"
    final val InnerClassesATTR: NameType           = "InnerClasses"
    final val MethodParametersATTR: NameType       = "MethodParameters"
    final val RuntimeAnnotationATTR: NameType      = "RuntimeVisibleAnnotations"   // RetentionPolicy.RUNTIME
    final val ScalaATTR: NameType                  = "Scala"
    final val ScalaSignatureATTR: NameType         = "ScalaSig"
    final val SignatureATTR: NameType              = "Signature"
    final val SourceFileATTR: NameType             = "SourceFile"
    final val SyntheticATTR: NameType              = "Synthetic"

    final val scala_ : NameType = "scala"

    def dropSingletonName(name: Name): TypeName = (name dropRight SINGLETON_SUFFIX.length).toTypeName
    def singletonName(name: Name): TypeName     = (name append SINGLETON_SUFFIX).toTypeName
  }

  abstract class TermNames extends Keywords with TermNamesApi {
    override type NameType = TermName

    protected implicit def createNameType(name: String): TermName = newTermNameCached(name)

    /** Base strings from which synthetic names are derived. */
    val BITMAP_PREFIX                  = "bitmap$"
    val CHECK_IF_REFUTABLE_STRING      = "check$ifrefutable$"
    val DEFAULT_GETTER_STRING          = "$default$"
    val DEFAULT_GETTER_INIT_STRING     = NameTransformer.encode("<init>") + DEFAULT_GETTER_STRING
    val DO_WHILE_PREFIX                = "doWhile$"
    val EVIDENCE_PARAM_PREFIX          = "evidence$"
    val EXCEPTION_RESULT_PREFIX        = "exceptionResult"
    val EXPAND_SEPARATOR_STRING        = "$$"
    val FRESH_TERM_NAME_PREFIX         = "x$"
    val INTERPRETER_IMPORT_WRAPPER     = "$iw"
    val LOCALDUMMY_PREFIX              = "<local "       // owner of local blocks
    val PROTECTED_PREFIX               = "protected$"
    val PROTECTED_SET_PREFIX           = PROTECTED_PREFIX + "set"
    val SUPER_PREFIX_STRING            = "super$"
    val WHILE_PREFIX                   = "while$"
    val FRESH_PREFIX                   = "fresh"
    val FRESH_SUFFIX                   = "macro$" // uses a keyword to avoid collisions with mangled names
    val QUAL_PREFIX                    = "qual$"
    val NAMEDARG_PREFIX                = "x$"

    // Compiler internal names
    val ANYname: NameType                  = "<anyname>"
    val CONSTRUCTOR: NameType              = "<init>"
    val DEFAULT_CASE: NameType             = "defaultCase$"
    val EQEQ_LOCAL_VAR: NameType           = "eqEqTemp$"
    val FAKE_LOCAL_THIS: NameType          = "this$"
    val LAZY_SLOW_SUFFIX: NameType         = "$lzycompute"
    val UNIVERSE_BUILD_PREFIX: NameType    = "$u.internal.reificationSupport."
    val UNIVERSE_PREFIX: NameType          = "$u."
    val UNIVERSE_SHORT: NameType           = "$u"
    val MIRROR_PREFIX: NameType            = "$m."
    val MIRROR_SHORT: NameType             = "$m"
    val MIRROR_UNTYPED: NameType           = "$m$untyped"
    val REIFY_FREE_PREFIX: NameType        = "free$"
    val REIFY_FREE_THIS_SUFFIX: NameType   = "$this"
    val REIFY_FREE_VALUE_SUFFIX: NameType  = "$value"
    val REIFY_SYMDEF_PREFIX: NameType      = "symdef$"
    val QUASIQUOTE_CASE: NameType          = "$quasiquote$case$"
    val QUASIQUOTE_EARLY_DEF: NameType     = "$quasiquote$early$def$"
    val QUASIQUOTE_FILE: String            = "<quasiquote>"
    val QUASIQUOTE_FOR_ENUM: NameType      = "$quasiquote$for$enum$"
    val QUASIQUOTE_NAME_PREFIX: String     = "nn$"
    val QUASIQUOTE_PACKAGE_STAT: NameType  = "$quasiquote$package$stat$"
    val QUASIQUOTE_PARAM: NameType         = "$quasiquote$param$"
    val QUASIQUOTE_PAT_DEF: NameType       = "$quasiquote$pat$def$"
    val QUASIQUOTE_PREFIX: String          = "qq$"
    val QUASIQUOTE_REFINE_STAT: NameType   = "$quasiquote$refine$stat$"
    val QUASIQUOTE_TUPLE: NameType         = "$quasiquote$tuple$"
    val QUASIQUOTE_UNLIFT_HELPER: String   = "$quasiquote$unlift$helper$"
    val MIXIN_CONSTRUCTOR: NameType        = "$init$"
    val MODULE_INSTANCE_FIELD: NameType    = NameTransformer.MODULE_INSTANCE_NAME  // "MODULE$"
    val OUTER: NameType                    = "$outer"
    val OUTER_LOCAL: NameType              = OUTER.localName
    val OUTER_ARG: NameType                = "arg" + OUTER
    val OUTER_SYNTH: NameType              = "<outer>" // emitted by virtual pattern matcher, replaced by outer accessor in explicitouter
    val ROOTPKG: NameType                  = "_root_"
    val SELECTOR_DUMMY: NameType           = "<unapply-selector>"
    val SELF: NameType                     = "$this"
    val SETTER_SUFFIX: NameType            = NameTransformer.SETTER_SUFFIX_STRING
    val SPECIALIZED_INSTANCE: NameType     = "specInstance$"
    val STAR: NameType                     = "*"
    val THIS: NameType                     = "_$this"

    def isConstructorName(name: Name)       = name == CONSTRUCTOR || name == MIXIN_CONSTRUCTOR
    def isExceptionResultName(name: Name)   = name startsWith EXCEPTION_RESULT_PREFIX
    def isLocalDummyName(name: Name)        = name startsWith LOCALDUMMY_PREFIX
    def isLocalName(name: Name)             = name endsWith LOCAL_SUFFIX_STRING
    def isLoopHeaderLabel(name: Name)       = (name startsWith WHILE_PREFIX) || (name startsWith DO_WHILE_PREFIX)
    def isProtectedAccessorName(name: Name) = name startsWith PROTECTED_PREFIX
    def isReplWrapperName(name: Name)       = name containsName INTERPRETER_IMPORT_WRAPPER
    def isSetterName(name: Name)            = name endsWith SETTER_SUFFIX
    def isTraitSetterName(name: Name)       = isSetterName(name) && (name containsName TRAIT_SETTER_SEPARATOR_STRING)
    def isSingletonName(name: Name)         = name endsWith SINGLETON_SUFFIX
    def isModuleName(name: Name)            = name endsWith MODULE_SUFFIX_NAME

    /** Is name a variable name? */
    def isVariableName(name: Name): Boolean = {
      val first = name.startChar
      (    ((first.isLower && first.isLetter) || first == '_')
        && (name != nme.false_)
        && (name != nme.true_)
        && (name != nme.null_)
      )
    }

    def isOpAssignmentName(name: Name) = name match {
      case raw.NE | raw.LE | raw.GE | EMPTY => false
      case _                                =>
      name.endChar == '=' && name.startChar != '=' && isOperatorPart(name.startChar)
    }

    private def expandedNameInternal(name: TermName, base: Symbol, separator: String): TermName =
      newTermNameCached(base.fullName('$') + separator + name)

    /** The expanded name of `name` relative to this class `base`
     */
    def expandedName(name: TermName, base: Symbol) = expandedNameInternal(name, base, EXPAND_SEPARATOR_STRING)

    /** The expanded setter name of `name` relative to this class `base`
    */
    def expandedSetterName(name: TermName, base: Symbol) = expandedNameInternal(name, base, TRAIT_SETTER_SEPARATOR_STRING)

    /** If `name` is an expandedName name, the original (unexpanded) name.
     *  Otherwise `name` itself.
     *  Look backward from the end of the string for "$$", and take the
     *  part of the string after that; but if the string is "$$$" or longer,
     *  be sure to retain the extra dollars.
     */
    def unexpandedName(name: Name): Name = name lastIndexOf "$$" match {
      case 0 | -1 => name
      case idx0   =>
        // Sketchville - We've found $$ but if it's part of $$$ or $$$$
        // or something we need to keep the bonus dollars, so e.g. foo$$$outer
        // has an original name of $outer.
        var idx = idx0
        while (idx > 0 && name.charAt(idx - 1) == '$')
          idx -= 1
        name drop idx + 2
    }

    @deprecated("use unexpandedName", "2.11.0") def originalName(name: Name): Name            = unexpandedName(name)
    @deprecated("use Name#dropModule", "2.11.0") def stripModuleSuffix(name: Name): Name      = name.dropModule
    @deprecated("use Name#dropLocal", "2.11.0") def localToGetter(name: TermName): TermName   = name.dropLocal
    @deprecated("use Name#dropLocal", "2.11.0") def dropLocalSuffix(name: Name): TermName     = name.dropLocal
    @deprecated("use Name#localName", "2.11.0") def getterToLocal(name: TermName): TermName   = name.localName
    @deprecated("use Name#setterName", "2.11.0") def getterToSetter(name: TermName): TermName = name.setterName
    @deprecated("use Name#getterName", "2.11.0") def getterName(name: TermName): TermName     = name.getterName
    @deprecated("use Name#getterName", "2.11.0") def setterToGetter(name: TermName): TermName = name.getterName

    /**
     * Convert `Tuple2$mcII` to `Tuple2`, or `T1$sp` to `T1`.
     */
    def unspecializedName(name: Name): Name = (
      // DUPLICATED LOGIC WITH `splitSpecializedName`
      if (name endsWith SPECIALIZED_SUFFIX)
        name.subName(0, name.lastIndexOf('m') - 1)
      else name
    )

    /** Return the original name and the types on which this name
    *  is specialized. For example,
    *  {{{
    *     splitSpecializedName("foo$mIcD$sp") == ('foo', "D", "I")
    *  }}}
    *  `foo$mIcD$sp` is the name of a method specialized on two type
    *  parameters, the first one belonging to the method itself, on Int,
    *  and another one belonging to the enclosing class, on Double.
    *
    *  @return (unspecializedName, class tparam specializations, method tparam specializations)
    */
    def splitSpecializedName(name: Name): (Name, String, String) =
      // DUPLICATED LOGIC WITH `unspecializedName`
    if (name endsWith SPECIALIZED_SUFFIX) {
      val name1 = name dropRight SPECIALIZED_SUFFIX.length
      val idxC  = name1 lastIndexOf 'c'
      val idxM  = name1 lastIndexOf 'm'

      (name1.subName(0, idxM - 1),
      name1.subName(idxC + 1, name1.length).toString,
      name1.subName(idxM + 1, idxC).toString)
    } else
    (name, "", "")

    // Nominally, name$default$N, encoded for <init>
    def defaultGetterName(name: Name, pos: Int): TermName = (
      if (isConstructorName(name))
        DEFAULT_GETTER_INIT_STRING + pos
      else
        name + DEFAULT_GETTER_STRING + pos
    )
    // Nominally, name from name$default$N, CONSTRUCTOR for <init>
    def defaultGetterToMethod(name: Name): TermName = (
      if (name startsWith DEFAULT_GETTER_INIT_STRING)
        nme.CONSTRUCTOR
      else name indexOf DEFAULT_GETTER_STRING match {
        case -1  => name.toTermName
        case idx => name.toTermName take idx
      }
    )

    def localDummyName(clazz: Symbol): TermName = newTermName(LOCALDUMMY_PREFIX + clazz.name + ">")
    def superName(name: Name, mix: Name = EMPTY): TermName = newTermName(SUPER_PREFIX_STRING + name + (if (mix.isEmpty) "" else "$" + mix))

    /** The name of an accessor for protected symbols. */
    def protName(name: Name): TermName = newTermName(PROTECTED_PREFIX + name)

    /** The name of a setter for protected symbols. Used for inherited Java fields. */
    def protSetterName(name: Name): TermName = newTermName(PROTECTED_SET_PREFIX + name)

    final val Nil: NameType                 = "Nil"
    final val Predef: NameType              = "Predef"

    val _1 : NameType  = "_1"
    val _2 : NameType  = "_2"
    val _3 : NameType  = "_3"
    val _4 : NameType  = "_4"
    val _5 : NameType  = "_5"
    val _6 : NameType  = "_6"
    val _7 : NameType  = "_7"
    val _8 : NameType  = "_8"
    val _9 : NameType  = "_9"
    val _10 : NameType = "_10"
    val _11 : NameType = "_11"
    val _12 : NameType = "_12"
    val _13 : NameType = "_13"
    val _14 : NameType = "_14"
    val _15 : NameType = "_15"
    val _16 : NameType = "_16"
    val _17 : NameType = "_17"
    val _18 : NameType = "_18"
    val _19 : NameType = "_19"
    val _20 : NameType = "_20"
    val _21 : NameType = "_21"
    val _22 : NameType = "_22"

    val x_0 : NameType  = "x$0"
    val x_1 : NameType  = "x$1"
    val x_2 : NameType  = "x$2"
    val x_3 : NameType  = "x$3"
    val x_4 : NameType  = "x$4"
    val x_5 : NameType  = "x$5"
    val x_6 : NameType  = "x$6"
    val x_7 : NameType  = "x$7"
    val x_8 : NameType  = "x$8"
    val x_9 : NameType  = "x$9"

    @switch def syntheticParamName(i: Int): TermName = i match {
      case 0  => nme.x_0
      case 1  => nme.x_1
      case 2  => nme.x_2
      case 3  => nme.x_3
      case 4  => nme.x_4
      case 5  => nme.x_5
      case 6  => nme.x_6
      case 7  => nme.x_7
      case 8  => nme.x_8
      case 9  => nme.x_9
      case _  => newTermName("x$" + i)
    }

    @switch def productAccessorName(j: Int): TermName = j match {
      case 1  => nme._1
      case 2  => nme._2
      case 3  => nme._3
      case 4  => nme._4
      case 5  => nme._5
      case 6  => nme._6
      case 7  => nme._7
      case 8  => nme._8
      case 9  => nme._9
      case 10 => nme._10
      case 11 => nme._11
      case 12 => nme._12
      case 13 => nme._13
      case 14 => nme._14
      case 15 => nme._15
      case 16 => nme._16
      case 17 => nme._17
      case 18 => nme._18
      case 19 => nme._19
      case 20 => nme._20
      case 21 => nme._21
      case 22 => nme._22
      case _  => newTermName("_" + j)
    }

    val ??? = encode("???")

    val wrapRefArray: NameType     = "wrapRefArray"
    val wrapByteArray: NameType    = "wrapByteArray"
    val wrapShortArray: NameType   = "wrapShortArray"
    val wrapCharArray: NameType    = "wrapCharArray"
    val wrapIntArray: NameType     = "wrapIntArray"
    val wrapLongArray: NameType    = "wrapLongArray"
    val wrapFloatArray: NameType   = "wrapFloatArray"
    val wrapDoubleArray: NameType  = "wrapDoubleArray"
    val wrapBooleanArray: NameType = "wrapBooleanArray"
    val wrapUnitArray: NameType    = "wrapUnitArray"
    val genericWrapArray: NameType = "genericWrapArray"

    // Compiler utilized names

    val AnnotatedType: NameType        = "AnnotatedType"
    val Annotation: NameType           = "Annotation"
    val Any: NameType                  = "Any"
    val AnyVal: NameType               = "AnyVal"
    val Apply: NameType                = "Apply"
    val ArrayAnnotArg: NameType        = "ArrayAnnotArg"
    val CaseDef: NameType              = "CaseDef"
    val ClassInfoType: NameType        = "ClassInfoType"
    val ConstantType: NameType         = "ConstantType"
    val EmptyPackage: NameType         = "EmptyPackage"
    val EmptyPackageClass: NameType    = "EmptyPackageClass"
    val ExistentialType: NameType      = "ExistentialType"
    val Flag : NameType                = "Flag"
    val FlagsRepr: NameType            = "FlagsRepr"
    val Ident: NameType                = "Ident"
    val ImplicitParams: NameType       = "ImplicitParams"
    val Import: NameType               = "Import"
    val Literal: NameType              = "Literal"
    val LiteralAnnotArg: NameType      = "LiteralAnnotArg"
    val MethodType: NameType           = "MethodType"
    val Modifiers: NameType            = "Modifiers"
    val NestedAnnotArg: NameType       = "NestedAnnotArg"
    val New: NameType                  = "New"
    val NoFlags: NameType              = "NoFlags"
    val NoSymbol: NameType             = "NoSymbol"
    val NoMods: NameType               = "NoMods"
    val Nothing: NameType              = "Nothing"
    val Null: NameType                 = "Null"
    val NullaryMethodType: NameType    = "NullaryMethodType"
    val Object: NameType               = "Object"
    val PolyType: NameType             = "PolyType"
    val RefinedType: NameType          = "RefinedType"
    val RootPackage: NameType          = "RootPackage"
    val RootClass: NameType            = "RootClass"
    val Select: NameType               = "Select"
    val SelectFromTypeTree: NameType   = "SelectFromTypeTree"
    val SingleType: NameType           = "SingleType"
    val SuperType: NameType            = "SuperType"
    val This: NameType                 = "This"
    val ThisType: NameType             = "ThisType"
    val Tuple2: NameType               = "Tuple2"
    val TYPE_ : NameType               = "TYPE"
    val TypeBounds: NameType           = "TypeBounds"
    val TypeRef: NameType              = "TypeRef"
    val TypeTree: NameType             = "TypeTree"
    val UNIT : NameType                = "UNIT"
    val accessor: NameType             = "accessor"
    val add_ : NameType                = "add"
    val annotation: NameType           = "annotation"
    val anyHash: NameType              = "anyHash"
    val anyValClass: NameType          = "anyValClass"
    val apply: NameType                = "apply"
    val applyDynamic: NameType         = "applyDynamic"
    val applyDynamicNamed: NameType    = "applyDynamicNamed"
    val applyOrElse: NameType          = "applyOrElse"
    val args : NameType                = "args"
    val arrayClass: NameType           = "arrayClass"
    val array_apply : NameType         = "array_apply"
    val array_clone : NameType         = "array_clone"
    val array_length : NameType        = "array_length"
    val array_update : NameType        = "array_update"
    val asModule: NameType             = "asModule"
    val asType: NameType               = "asType"
    val asInstanceOf_ : NameType       = "asInstanceOf"
    val asInstanceOf_Ob : NameType     = "$asInstanceOf"
    val box: NameType                  = "box"
    val bytes: NameType                = "bytes"
    val c: NameType                    = "c"
    val canEqual_ : NameType           = "canEqual"
    val classOf: NameType              = "classOf"
    val clone_ : NameType              = "clone"
    val collection: NameType           = "collection"
    val conforms: NameType             = "$conforms" // dollar prefix to avoid accidental shadowing
    val copy: NameType                 = "copy"
    val create: NameType               = "create"
    val currentMirror: NameType        = "currentMirror"
    val delayedInit: NameType          = "delayedInit"
    val delayedInitArg: NameType       = "delayedInit$body"
    val dollarScope: NameType          = "$scope"
    val doubleHash: NameType           = "doubleHash"
    val drop: NameType                 = "drop"
    val elem: NameType                 = "elem"
    val noSelfType: NameType           = "noSelfType"
    val ensureAccessible : NameType    = "ensureAccessible"
    val eq: NameType                   = "eq"
    val equalsNumChar : NameType       = "equalsNumChar"
    val equalsNumNum : NameType        = "equalsNumNum"
    val equalsNumObject : NameType     = "equalsNumObject"
    val equals_ : NameType             = "equals"
    val error: NameType                = "error"
    val ex: NameType                   = "ex"
    val experimental: NameType         = "experimental"
    val f: NameType                    = "f"
    val false_ : NameType              = "false"
    val filter: NameType               = "filter"
    val finalize_ : NameType           = "finalize"
    val find_ : NameType               = "find"
    val flatMap: NameType              = "flatMap"
    val floatHash: NameType            = "floatHash"
    val foreach: NameType              = "foreach"
    val freshTermName: NameType        = "freshTermName"
    val freshTypeName: NameType        = "freshTypeName"
    val get: NameType                  = "get"
    val parameterTypes: NameType       = "parameterTypes"
    val hashCode_ : NameType           = "hashCode"
    val head : NameType                = "head"
    val immutable: NameType            = "immutable"
    val implicitly: NameType           = "implicitly"
    val in: NameType                   = "in"
    val initialize : NameType          = "initialize"
    val initialized : NameType         = "initialized"
    val internal: NameType             = "internal"
    val inlinedEquals: NameType        = "inlinedEquals"
    val isArray: NameType              = "isArray"
    val isDefinedAt: NameType          = "isDefinedAt"
    val isEmpty: NameType              = "isEmpty"
    val isInstanceOf_ : NameType       = "isInstanceOf"
    val isInstanceOf_Ob : NameType     = "$isInstanceOf"
    val java: NameType                 = "java"
    val key: NameType                  = "key"
    val lang: NameType                 = "lang"
    val length: NameType               = "length"
    val lengthCompare: NameType        = "lengthCompare"
    val longHash: NameType             = "longHash"
    val macroContext : NameType        = "c"
    val main: NameType                 = "main"
    val manifestToTypeTag: NameType    = "manifestToTypeTag"
    val map: NameType                  = "map"
    val materializeClassTag: NameType  = "materializeClassTag"
    val materializeWeakTypeTag: NameType = "materializeWeakTypeTag"
    val materializeTypeTag: NameType   = "materializeTypeTag"
    val moduleClass : NameType         = "moduleClass"
    val mkAnnotation: NameType         = "mkAnnotation"
    val mkEarlyDef: NameType           = "mkEarlyDef"
    val mkIdent: NameType              = "mkIdent"
    val mkPackageStat: NameType        = "mkPackageStat"
    val mkRefineStat: NameType         = "mkRefineStat"
    val mkRefTree: NameType            = "mkRefTree"
    val mkSelect: NameType             = "mkSelect"
    val mkThis: NameType               = "mkThis"
    val mkTypeTree: NameType           = "mkTypeTree"
    val ne: NameType                   = "ne"
    val newArray: NameType             = "newArray"
    val newFreeTerm: NameType          = "newFreeTerm"
    val newFreeType: NameType          = "newFreeType"
    val newNestedSymbol: NameType      = "newNestedSymbol"
    val newScopeWith: NameType         = "newScopeWith"
    val notifyAll_ : NameType          = "notifyAll"
    val notify_ : NameType             = "notify"
    val null_ : NameType               = "null"
    val pendingSuperCall: NameType     = "pendingSuperCall"
    val prefix : NameType              = "prefix"
    val productArity: NameType         = "productArity"
    val productElement: NameType       = "productElement"
    val productIterator: NameType      = "productIterator"
    val productPrefix: NameType        = "productPrefix"
    val readResolve: NameType          = "readResolve"
    val reify : NameType               = "reify"
    val reificationSupport : NameType  = "reificationSupport"
    val rootMirror : NameType          = "rootMirror"
    val runtime: NameType              = "runtime"
    val runtimeClass: NameType         = "runtimeClass"
    val runtimeMirror: NameType        = "runtimeMirror"
    val scala_ : NameType              = "scala"
    val selectDynamic: NameType        = "selectDynamic"
    val selectOverloadedMethod: NameType = "selectOverloadedMethod"
    val selectTerm: NameType           = "selectTerm"
    val selectType: NameType           = "selectType"
    val self: NameType                 = "self"
    val setAnnotations: NameType       = "setAnnotations"
    val setInfo: NameType              = "setInfo"
    val setSymbol: NameType            = "setSymbol"
    val setType: NameType              = "setType"
    val splice: NameType               = "splice"
    val staticClass : NameType         = "staticClass"
    val staticModule : NameType        = "staticModule"
    val staticPackage : NameType       = "staticPackage"
    val synchronized_ : NameType       = "synchronized"
    val ScalaDot: NameType             = "ScalaDot"
    val TermName: NameType             = "TermName"
    val this_ : NameType               = "this"
    val thisPrefix : NameType          = "thisPrefix"
    val toArray: NameType              = "toArray"
    val toList: NameType               = "toList"
    val toObjectArray : NameType       = "toObjectArray"
    val toStats: NameType              = "toStats"
    val TopScope: NameType             = "TopScope"
    val toString_ : NameType           = "toString"
    val toTypeConstructor: NameType    = "toTypeConstructor"
    val tpe : NameType                 = "tpe"
    val tree : NameType                = "tree"
    val true_ : NameType               = "true"
    val typedProductIterator: NameType = "typedProductIterator"
    val TypeName: NameType             = "TypeName"
    val typeTagToManifest: NameType    = "typeTagToManifest"
    val unapply: NameType              = "unapply"
    val unapplySeq: NameType           = "unapplySeq"
    val unbox: NameType                = "unbox"
    val universe: NameType             = "universe"
    val UnliftListElementwise: NameType = "UnliftListElementwise"
    val UnliftListOfListsElementwise: NameType = "UnliftListOfListsElementwise"
    val update: NameType               = "update"
    val updateDynamic: NameType        = "updateDynamic"
    val value: NameType                = "value"
    val valueOf : NameType             = "valueOf"
    val values : NameType              = "values"
    val wait_ : NameType               = "wait"
    val withFilter: NameType           = "withFilter"
    val xml: NameType                  = "xml"
    val zero: NameType                 = "zero"

    // quasiquote interpolators:
    val q: NameType  = "q"
    val tq: NameType = "tq"
    val cq: NameType = "cq"
    val pq: NameType = "pq"
    val fq: NameType = "fq"

    // quasiquote's syntactic combinators
    val SyntacticAnnotatedType: NameType    = "SyntacticAnnotatedType"
    val SyntacticApplied: NameType          = "SyntacticApplied"
    val SyntacticAppliedType: NameType      = "SyntacticAppliedType"
    val SyntacticAssign: NameType           = "SyntacticAssign"
    val SyntacticBlock: NameType            = "SyntacticBlock"
    val SyntacticClassDef: NameType         = "SyntacticClassDef"
    val SyntacticCompoundType: NameType     = "SyntacticCompoundType"
    val SyntacticDefDef: NameType           = "SyntacticDefDef"
    val SyntacticEmptyTypeTree: NameType    = "SyntacticEmptyTypeTree"
    val SyntacticExistentialType: NameType  = "SyntacticExistentialType"
    val SyntacticFilter: NameType           = "SyntacticFilter"
    val SyntacticFor: NameType              = "SyntacticFor"
    val SyntacticForYield: NameType         = "SyntacticForYield"
    val SyntacticFunction: NameType         = "SyntacticFunction"
    val SyntacticFunctionType: NameType     = "SyntacticFunctionType"
    val SyntacticImport: NameType           = "SyntacticImport"
    val SyntacticMatch: NameType            = "SyntacticMatch"
    val SyntacticNew: NameType              = "SyntacticNew"
    val SyntacticObjectDef: NameType        = "SyntacticObjectDef"
    val SyntacticPackageObjectDef: NameType = "SyntacticPackageObjectDef"
    val SyntacticPartialFunction: NameType  = "SyntacticPartialFunction"
    val SyntacticPatDef: NameType           = "SyntacticPatDef"
    val SyntacticSelectTerm: NameType       = "SyntacticSelectTerm"
    val SyntacticSelectType: NameType       = "SyntacticSelectType"
    val SyntacticSingletonType: NameType    = "SyntacticSingletonType"
    val SyntacticTermIdent: NameType        = "SyntacticTermIdent"
    val SyntacticTraitDef: NameType         = "SyntacticTraitDef"
    val SyntacticTry: NameType              = "SyntacticTry"
    val SyntacticTuple: NameType            = "SyntacticTuple"
    val SyntacticTupleType: NameType        = "SyntacticTupleType"
    val SyntacticTypeApplied: NameType      = "SyntacticTypeApplied"
    val SyntacticTypeIdent: NameType        = "SyntacticTypeIdent"
    val SyntacticTypeProjection: NameType   = "SyntacticTypeProjection"
    val SyntacticValDef: NameType           = "SyntacticValDef"
    val SyntacticValEq: NameType            = "SyntacticValEq"
    val SyntacticValFrom: NameType          = "SyntacticValFrom"
    val SyntacticVarDef: NameType           = "SyntacticVarDef"

    // unencoded operators
    object raw {
      final val BANG : NameType  = "!"
      final val BAR  : NameType  = "|"
      final val DOLLAR: NameType = "$"
      final val GE: NameType     = ">="
      final val LE: NameType     = "<="
      final val MINUS: NameType  = "-"
      final val NE: NameType     = "!="
      final val PLUS : NameType  = "+"
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

    // primitive operation methods for structural types mostly
    // overlap with the above, but not for these two.
    val toCharacter: NameType = "toCharacter"
    val toInteger: NameType   = "toInteger"

    def newLazyValSlowComputeName(lzyValName: Name) = (lzyValName stripSuffix MODULE_VAR_SUFFIX append LAZY_SLOW_SUFFIX).toTermName

    // ASCII names for operators
    val ADD       = encode("+")
    val AND       = encode("&")
    val ASR       = encode(">>")
    val CONS      = encode("::")
    val COLONPLUS = encode(":+")
    val DIV       = encode("/")
    val EQ        = encode("==")
    val EQL       = encode("=")
    val GE        = encode(">=")
    val GT        = encode(">")
    val HASHHASH  = encode("##")
    val LE        = encode("<=")
    val LSL       = encode("<<")
    val LSR       = encode(">>>")
    val LT        = encode("<")
    val MINUS     = encode("-")
    val MINGT     = encode("->")
    val MOD       = encode("%")
    val MUL       = encode("*")
    val NE        = encode("!=")
    val OR        = encode("|")
    val PLUS      = ADD    // technically redundant, but ADD looks funny with MINUS
    val PLUSPLUS  = encode("++")
    val SUB       = MINUS  // ... as does SUB with PLUS
    val XOR       = encode("^")
    val ZAND      = encode("&&")
    val ZOR       = encode("||")

    // unary operators
    val UNARY_~ = encode("unary_~")
    val UNARY_+ = encode("unary_+")
    val UNARY_- = encode("unary_-")
    val UNARY_! = encode("unary_!")

    // Grouped here so Cleanup knows what tests to perform.
    val CommonOpNames   = Set[Name](OR, XOR, AND, EQ, NE)
    val BooleanOpNames  = Set[Name](ZOR, ZAND, UNARY_!) ++ CommonOpNames

    val add: NameType                    = "add"
    val complement: NameType             = "complement"
    val divide: NameType                 = "divide"
    val multiply: NameType               = "multiply"
    val negate: NameType                 = "negate"
    val positive: NameType               = "positive"
    val shiftLogicalRight: NameType      = "shiftLogicalRight"
    val shiftSignedLeft: NameType        = "shiftSignedLeft"
    val shiftSignedRight: NameType       = "shiftSignedRight"
    val subtract: NameType               = "subtract"
    val takeAnd: NameType                = "takeAnd"
    val takeConditionalAnd: NameType     = "takeConditionalAnd"
    val takeConditionalOr: NameType      = "takeConditionalOr"
    val takeModulo: NameType             = "takeModulo"
    val takeNot: NameType                = "takeNot"
    val takeOr: NameType                 = "takeOr"
    val takeXor: NameType                = "takeXor"
    val testEqual: NameType              = "testEqual"
    val testGreaterOrEqualThan: NameType = "testGreaterOrEqualThan"
    val testGreaterThan: NameType        = "testGreaterThan"
    val testLessOrEqualThan: NameType    = "testLessOrEqualThan"
    val testLessThan: NameType           = "testLessThan"
    val testNotEqual: NameType           = "testNotEqual"

    def toUnaryName(name: TermName): TermName = name match {
      case raw.MINUS => UNARY_-
      case raw.PLUS  => UNARY_+
      case raw.TILDE => UNARY_~
      case raw.BANG  => UNARY_!
      case _         => name
    }
    /** The name of a method which stands in for a primitive operation
     *  during structural type dispatch.
     */
    def primitiveInfixMethodName(name: Name): TermName = name match {
      case OR   => takeOr
      case XOR  => takeXor
      case AND  => takeAnd
      case EQ   => testEqual
      case NE   => testNotEqual
      case ADD  => add
      case SUB  => subtract
      case MUL  => multiply
      case DIV  => divide
      case MOD  => takeModulo
      case LSL  => shiftSignedLeft
      case LSR  => shiftLogicalRight
      case ASR  => shiftSignedRight
      case LT   => testLessThan
      case LE   => testLessOrEqualThan
      case GE   => testGreaterOrEqualThan
      case GT   => testGreaterThan
      case ZOR  => takeConditionalOr
      case ZAND => takeConditionalAnd
      case _    => NO_NAME
    }
    /** Postfix/prefix, really.
     */
    def primitivePostfixMethodName(name: Name): TermName = name match {
      case UNARY_!    => takeNot
      case UNARY_+    => positive
      case UNARY_-    => negate
      case UNARY_~    => complement
      case `toByte`   => toByte
      case `toShort`  => toShort
      case `toChar`   => toCharacter
      case `toInt`    => toInteger
      case `toLong`   => toLong
      case `toFloat`  => toFloat
      case `toDouble` => toDouble
      case _          => NO_NAME
    }

    def primitiveMethodName(name: Name): TermName =
      primitiveInfixMethodName(name) match {
        case NO_NAME => primitivePostfixMethodName(name)
        case name => name
      }

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
          val (simple, div, rest) = (name take idx, name charAt idx, name drop idx + 1)
          mkName(simple, div == '.') :: segments(rest, assumeTerm)
      }
    }

    def newBitmapName(bitmapPrefix: Name, n: Int) = bitmapPrefix append ("" + n)

    val BITMAP_NORMAL: NameType              = BITMAP_PREFIX + ""           // initialization bitmap for public/protected lazy vals
    val BITMAP_TRANSIENT: NameType           = BITMAP_PREFIX + "trans$"     // initialization bitmap for transient lazy vals
    val BITMAP_CHECKINIT: NameType           = BITMAP_PREFIX + "init$"      // initialization bitmap for checkinit values
    val BITMAP_CHECKINIT_TRANSIENT: NameType = BITMAP_PREFIX + "inittrans$" // initialization bitmap for transient checkinit values
  }

  lazy val typeNames: tpnme.type = tpnme

  object tpnme extends TypeNames { }

  /** For fully qualified type names.
   */
  object fulltpnme extends TypeNames {
    val RuntimeNothing: NameType = "scala.runtime.Nothing$"
    val RuntimeNull: NameType    = "scala.runtime.Null$"
  }

  /** Java binary names, like scala/runtime/Nothing$.
   */
  object binarynme {
    def toBinary(name: Name) = name mapName (_.replace('.', '/'))

    val RuntimeNothing = toBinary(fulltpnme.RuntimeNothing).toTypeName
    val RuntimeNull    = toBinary(fulltpnme.RuntimeNull).toTypeName
  }

  val javanme = nme.javaKeywords

  lazy val termNames: nme.type = nme

  object nme extends TermNames {
    def moduleVarName(name: TermName): TermName =
      newTermNameCached("" + name + MODULE_VAR_SUFFIX)

    def getCause         = sn.GetCause
    def getClass_        = sn.GetClass
    def getMethod_       = sn.GetMethod
    def invoke_          = sn.Invoke

    val isBoxedNumberOrBoolean: NameType = "isBoxedNumberOrBoolean"
    val isBoxedNumber: NameType = "isBoxedNumber"

    val reflPolyCacheName: NameType   = "reflPoly$Cache"
    val reflParamsCacheName: NameType = "reflParams$Cache"
    val reflMethodName: NameType      = "reflMethod$Method"
    val argument: NameType            = "<argument>"

  }

  class JavaKeywords {
    private val kw = new KeywordSetBuilder

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

    final val keywords = kw.result
  }

  sealed abstract class SymbolNames {
    protected val stringToTermName = null
    protected val stringToTypeName = null
    protected implicit def createNameType(s: String): TypeName = newTypeNameCached(s)

    final val BoxedBoolean: TypeName       = "java.lang.Boolean"
    final val BoxedByte: TypeName          = "java.lang.Byte"
    final val BoxedCharacter: TypeName     = "java.lang.Character"
    final val BoxedDouble: TypeName        = "java.lang.Double"
    final val BoxedFloat: TypeName         = "java.lang.Float"
    final val BoxedInteger: TypeName       = "java.lang.Integer"
    final val BoxedLong: TypeName          = "java.lang.Long"
    final val BoxedNumber: TypeName        = "java.lang.Number"
    final val BoxedShort: TypeName         = "java.lang.Short"
    final val IOOBException: TypeName      = "java.lang.IndexOutOfBoundsException"
    final val InvTargetException: TypeName = "java.lang.reflect.InvocationTargetException"
    final val MethodAsObject: TypeName     = "java.lang.reflect.Method"
    final val NPException: TypeName        = "java.lang.NullPointerException"
    final val Object: TypeName             = "java.lang.Object"
    final val Throwable: TypeName          = "java.lang.Throwable"

    final val GetCause: TermName         = newTermName("getCause")
    final val GetClass: TermName         = newTermName("getClass")
    final val GetClassLoader: TermName   = newTermName("getClassLoader")
    final val GetMethod: TermName        = newTermName("getMethod")
    final val Invoke: TermName           = newTermName("invoke")
    final val InvokeExact: TermName      = newTermName("invokeExact")

    final val Metafactory: TermName         = newTermName("metafactory")
    final val AltMetafactory: TermName      = newTermName("altMetafactory")
    final val Bootstrap: TermName           = newTermName("bootstrap")

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

  lazy val sn: SymbolNames = new SymbolNames { }
}
