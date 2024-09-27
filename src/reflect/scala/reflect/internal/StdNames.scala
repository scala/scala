/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal

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
    private[this] var kws: Set[TermName] = Set()
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
     * The maximum length of a filename on some platforms is 240 chars (docker).
     * Therefore, compactify names that would create a filename longer than that.
     * A compactified name looks like
     *     prefix + $$$$ + md5 + $$$$ + suffix,
     * where the prefix and suffix are the first and last quarter of the name,
     * respectively.
     *
     * So how long is too long? For a (flattened class) name, the resulting file
     * will be called "name.class", or, if it's a module class, "name$.class"
     * (see scala/bug#8199). Therefore the maximum suffix is 7 characters, and
     * names that are over (240 - 7) characters get compactified.
     */
    final val marker          = "$$$$"
    final val MaxSuffixLength = 7 // "$.class".length + 1 // potential module class suffix and file extension
    final val MaxNameLength   = 240 - MaxSuffixLength
    def toMD5(s: String, edge: Int): String = {
      val prefix = s take edge
      val suffix = s takeRight edge

      val cs = s.toArray
      val bytes = Codec.toUTF8(new scala.runtime.ArrayCharSequence(cs, 0, cs.length))
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
    protected def nameType(name: String): NameType

    def flattenedName(owner: Symbol, name: Name): NameType = {
      val flat = owner.name.toString + NAME_JOIN_STRING + name.toString
      val nameString = if (owner.isJava) flat else compactify(flat) // scala/bug#11277
      nameType(nameString)
    }

    // TODO: what is the purpose of all this duplication!?!?!
    // I made these constants because we cannot change them without bumping our major version anyway.
    final val NAME_JOIN_STRING                 = NameTransformer.NAME_JOIN_STRING
    final val MODULE_SUFFIX_STRING             = NameTransformer.MODULE_SUFFIX_STRING
    final val MODULE_VAR_SUFFIX_STRING         = NameTransformer.MODULE_VAR_SUFFIX_STRING
    final val LOCAL_SUFFIX_STRING              = NameTransformer.LOCAL_SUFFIX_STRING
    final val LAZY_LOCAL_SUFFIX_STRING         = NameTransformer.LAZY_LOCAL_SUFFIX_STRING
    final val TRAIT_SETTER_SEPARATOR_STRING    = NameTransformer.TRAIT_SETTER_SEPARATOR_STRING
    final val SINGLETON_SUFFIX                 = ".type"

    val ANON_CLASS_NAME: NameType              = nameType("$anon")
    val DELAMBDAFY_LAMBDA_CLASS_NAME: NameType = nameType("$lambda")
    val ANON_FUN_NAME: NameType                = nameType("$anonfun")
    val EMPTY: NameType                        = nameType("")
    val EMPTY_PACKAGE_NAME: NameType           = nameType("<empty>")
    val IMPORT: NameType                       = nameType("<import>")
    val MODULE_SUFFIX_NAME: NameType           = nameType(MODULE_SUFFIX_STRING)
    val MODULE_VAR_SUFFIX: NameType            = nameType(MODULE_VAR_SUFFIX_STRING)
    val PACKAGE: NameType                      = nameType("package")
    val ROOT: NameType                         = nameType("<root>")
    val SPECIALIZED_SUFFIX: NameType           = nameType("$sp")
    val CASE_ACCESSOR: NameType                = nameType("$access")

    val NESTED_IN: String                      = "$nestedIn"
    val NESTED_IN_ANON_CLASS: String           = NESTED_IN + ANON_CLASS_NAME.toString.replace("$", "")
    val NESTED_IN_ANON_FUN: String             = NESTED_IN + ANON_FUN_NAME.toString.replace("$", "")
    val NESTED_IN_LAMBDA: String               = NESTED_IN + DELAMBDAFY_LAMBDA_CLASS_NAME.toString.replace("$", "")

    val NON_LOCAL_RETURN_KEY_STRING: String    = "nonLocalReturnKey"
    val LIFTED_TREE: String                    = "liftedTree"

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
    final val Boolean: NameType = nameType("Boolean")
    final val Byte: NameType    = nameType("Byte")
    final val Char: NameType    = nameType("Char")
    final val Double: NameType  = nameType("Double")
    final val Float: NameType   = nameType("Float")
    final val Int: NameType     = nameType("Int")
    final val Long: NameType    = nameType("Long")
    final val Short: NameType   = nameType("Short")
    final val Unit: NameType    = nameType("Unit")

    // some types whose companions we utilize
    final val AnyRef: NameType        = nameType("AnyRef")
    final val Array: NameType         = nameType("Array")
    final val List: NameType          = nameType("List")
    final val Option: NameType        = nameType("Option")
    final val Seq: NameType           = nameType("Seq")
    final val Symbol: NameType        = nameType("Symbol")
    final val WeakTypeTag: NameType   = nameType("WeakTypeTag")
    final val TypeTag : NameType      = nameType("TypeTag")
    final val Expr: NameType          = nameType("Expr")
    final val String: NameType        = nameType("String")

    // some names whose name we utilize
    final val StringContextName: NameType = nameType("StringContext")

    // fictions we use as both types and terms
    final val ERROR: NameType    = nameType("<error>")
    final val NO_NAME: NameType  = nameType("<none>")  // formerly NOSYMBOL
    final val WILDCARD: NameType = nameType("_")
  }

  // FIXME: This class requires early initializers to work, which are deprecated
  //        and will not be supported in 3.0. Please change the design and remove
  //        the early initializer.
  /** This should be the first trait in the linearization. */
  // abstract class Keywords extends CommonNames {
  abstract class Keywords extends {
    private[this] val kw = new KeywordSetBuilder

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
    final val javaRestrictedIdentifiers = new JavaRestrictedIdentifiers()
  }

  abstract class TypeNames extends Keywords with TypeNamesApi {
    override type NameType = TypeName

    protected def nameType(name: String): TypeName = newTypeNameCached(name)

    final val BYNAME_PARAM_CLASS_NAME: NameType        = nameType("<byname>")
    final val JAVA_REPEATED_PARAM_CLASS_NAME: NameType = nameType("<repeated...>")
    final val LOCAL_CHILD: NameType                    = nameType("<local child>")
    final val REFINE_CLASS_NAME: NameType              = nameType("<refinement>")
    final val REPEATED_PARAM_CLASS_NAME: NameType      = nameType("<repeated>")
    final val WILDCARD_STAR: NameType                  = nameType("_*")
    final val REIFY_TREECREATOR_PREFIX: NameType       = nameType("$treecreator")
    final val REIFY_TYPECREATOR_PREFIX: NameType       = nameType("$typecreator")
    final val MACRO_BUNDLE_SUFFIX: NameType            = nameType("$Bundle")

    final val Any: NameType             = nameType("Any")
    final val AnyVal: NameType          = nameType("AnyVal")
    final val App: NameType             = nameType("App")
    final val FlagSet: NameType         = nameType("FlagSet")
    final val Mirror: NameType          = nameType("Mirror")
    final val Modifiers: NameType       = nameType("Modifiers")
    final val Nothing: NameType         = nameType("Nothing")
    final val Null: NameType            = nameType("Null")
    final val Object: NameType          = nameType("Object")
    final val PrefixType: NameType      = nameType("PrefixType")
    final val Product: NameType         = nameType("Product")
    final val Record: NameType          = nameType("Record")
    final val Serializable: NameType    = nameType("Serializable")
    final val Singleton: NameType       = nameType("Singleton")
    final val Throwable: NameType       = nameType("Throwable")
    final val unchecked: NameType       = nameType("unchecked")
    final val ValueOf: NameType         = nameType("ValueOf")

    final val api: NameType                 = nameType("api")
    final val Annotation: NameType          = nameType("Annotation")
    final val CaseDef: NameType             = nameType("CaseDef")
    final val ClassManifest: NameType       = nameType("ClassManifest")
    final val Enum: NameType                = nameType("Enum")
    final val Group: NameType               = nameType("Group")
    final val implicitNotFound: NameType    = nameType("implicitNotFound")
    final val Liftable: NameType            = nameType("Liftable")
    final val Unliftable: NameType          = nameType("Unliftable")
    final val Name: NameType                = nameType("Name")
    final val StaticAnnotation: NameType    = nameType("StaticAnnotation")
    final val Tree: NameType                = nameType("Tree")
    final val Text: NameType                = nameType("Text")
    final val TermName: NameType            = nameType("TermName")
    final val Type : NameType               = nameType("Type")
    final val TypeName: NameType            = nameType("TypeName")
    final val TypeDef: NameType             = nameType("TypeDef")
    final val Quasiquote: NameType          = nameType("Quasiquote")
    final val macroImplLocation: NameType   = nameType("macroImplLocation")
    final val UnapplySeqWrapper: NameType   = nameType("UnapplySeqWrapper")

    // async
    final val stateMachine: NameType        = nameType("stateMachine$async")

    // quasiquote-specific names
    final val QUASIQUOTE_FUNCTION: NameType     = nameType("$quasiquote$function$")
    final val QUASIQUOTE_MODS: NameType         = nameType("$quasiquote$mods$")
    final val QUASIQUOTE_TUPLE: NameType        = nameType("$quasiquote$tuple$")

    // Annotation simple names, used in Namer
    final val BeanPropertyAnnot: NameType        = nameType("BeanProperty")
    final val BooleanBeanPropertyAnnot: NameType = nameType("BooleanBeanProperty")

    // Classfile Attributes
    final val AnnotationDefaultATTR: NameType      = nameType("AnnotationDefault")
    final val BridgeATTR: NameType                 = nameType("Bridge")
    final val CodeATTR: NameType                   = nameType("Code")
    final val ConstantValueATTR: NameType          = nameType("ConstantValue")
    final val DeprecatedATTR: NameType             = nameType("Deprecated")
    final val ExceptionsATTR: NameType             = nameType("Exceptions")
    final val InnerClassesATTR: NameType           = nameType("InnerClasses")
    final val MethodParametersATTR: NameType       = nameType("MethodParameters")
    final val RuntimeAnnotationATTR: NameType      = nameType("RuntimeVisibleAnnotations") // RetentionPolicy.RUNTIME
    final val ScalaATTR: NameType                  = nameType("Scala")
    final val TASTYATTR: NameType                  = nameType("TASTY")
    final val ScalaSignatureATTR: NameType         = nameType("ScalaSig")
    final val SignatureATTR: NameType              = nameType("Signature")
    final val SourceFileATTR: NameType             = nameType("SourceFile")
    final val SyntheticATTR: NameType              = nameType("Synthetic")
    final val PermittedSubclassesATTR: NameType    = nameType("PermittedSubclasses")

    final val scala_ : NameType = nameType("scala")

    // Scala 3 special type
    val AND: NameType = nme.AND.toTypeName

    def dropSingletonName(name: Name): TypeName = (name dropRight SINGLETON_SUFFIX.length).toTypeName
    def singletonName(name: Name): TypeName     = (name append SINGLETON_SUFFIX).toTypeName
  }

  abstract class TermNames extends Keywords with TermNamesApi {
    override type NameType = TermName

    protected def nameType(name: String): TermName = newTermNameCached(name)

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
    val INTERPRETER_IMPORT_LEVEL_UP    = NameTransformer.encode("{{")
    val INTERPRETER_IMPORT_WRAPPER     = "$iw"
    val INTERPRETER_WRAPPER            = "$read"
    val LOCALDUMMY_PREFIX              = "<local "       // owner of local blocks
    val PROTECTED_PREFIX               = "protected$"
    val PROTECTED_SET_PREFIX           = PROTECTED_PREFIX + "set"
    val SUPER_PREFIX_STRING            = "super$"
    val WHILE_PREFIX                   = "while$"
    val FRESH_PREFIX                   = "fresh"
    val FRESH_SUFFIX                   = "macro$" // uses a keyword to avoid collisions with mangled names
    val QUAL_PREFIX                    = "qual$"
    val NAMEDARG_PREFIX                = "x$"
    val RIGHT_ASSOC_OP_PREFIX          = "rassoc$"
    val STABILIZER_PREFIX              = "stabilizer$"

    // Compiler internal names
    val ANYname: NameType                  = nameType("<anyname>")
    val CONSTRUCTOR: NameType              = nameType("<init>")
    val CLASS_CONSTRUCTOR: NameType        = nameType("<clinit>")
    val DEFAULT_CASE: NameType             = nameType("defaultCase$")
    val EQEQ_LOCAL_VAR: NameType           = nameType("eqEqTemp$")
    val FAKE_LOCAL_THIS: NameType          = nameType("this$")
    val LAZY_SLOW_SUFFIX: NameType         = nameType("$lzycompute")
    val UNIVERSE_BUILD_PREFIX: NameType    = nameType("$u.internal.reificationSupport.")
    val UNIVERSE_PREFIX: NameType          = nameType("$u.")
    val UNIVERSE_SHORT: NameType           = nameType("$u")
    val MIRROR_PREFIX: NameType            = nameType("$m.")
    val MIRROR_SHORT: NameType             = nameType("$m")
    val MIRROR_UNTYPED: NameType           = nameType("$m$untyped")
    val REIFY_FREE_PREFIX: NameType        = nameType("free$")
    val REIFY_FREE_THIS_SUFFIX: NameType   = nameType(s"$$this")
    val REIFY_FREE_VALUE_SUFFIX: NameType  = nameType(s"$$value") // looks like missing interpolator due to `value` in scope
    val REIFY_SYMDEF_PREFIX: NameType      = nameType("symdef$")
    val QUASIQUOTE_CASE: NameType          = nameType("$quasiquote$case$")
    val QUASIQUOTE_EARLY_DEF: NameType     = nameType("$quasiquote$early$def$")
    val QUASIQUOTE_FILE: String            = "<quasiquote>"
    val QUASIQUOTE_FOR_ENUM: NameType      = nameType("$quasiquote$for$enum$")
    val QUASIQUOTE_NAME_PREFIX: String     = "nn$"
    val QUASIQUOTE_PACKAGE_STAT: NameType  = nameType("$quasiquote$package$stat$")
    val QUASIQUOTE_PARAM: NameType         = nameType("$quasiquote$param$")
    val QUASIQUOTE_PAT_DEF: NameType       = nameType("$quasiquote$pat$def$")
    val QUASIQUOTE_PREFIX: String          = "qq$"
    val QUASIQUOTE_REFINE_STAT: NameType   = nameType("$quasiquote$refine$stat$")
    val QUASIQUOTE_TUPLE: NameType         = nameType("$quasiquote$tuple$")
    val QUASIQUOTE_UNLIFT_HELPER: String   = "$quasiquote$unlift$helper$"
    val MIXIN_CONSTRUCTOR: NameType        = nameType("$init$")
    val MODULE_INSTANCE_FIELD: NameType    = nameType(NameTransformer.MODULE_INSTANCE_NAME) // "MODULE$"
    val OUTER: NameType                    = nameType("$outer")
    val OUTER_LOCAL: NameType              = OUTER.localName
    val OUTER_ARG: NameType                = nameType("arg" + OUTER)
    val OUTER_SYNTH: NameType              = nameType("<outer>") // emitted by pattern matcher, replaced by outer accessor in explicitouter
    val ROOTPKG: NameType                  = nameType("_root_")
    val SELECTOR_DUMMY: NameType           = nameType("<unapply-selector>")
    val SELF: NameType                     = nameType(s"$$this")
    val SETTER_SUFFIX: NameType            = nameType(NameTransformer.SETTER_SUFFIX_STRING)
    val SPECIALIZED_INSTANCE: NameType     = nameType("specInstance$")
    val STAR: NameType                     = nameType("*")
    val THIS: NameType                     = nameType(s"_$$this")


    val annottees: NameType               = nameType("annottees")       // for macro annotations
    val macroTransform: NameType          = nameType("macroTransform")  // for macro annotations
    val unpickledMacroImpl: NameType      = nameType("unpickledMacroImpl") // for tasty macro unpickling

    def isConstructorName(name: Name)       = name == CONSTRUCTOR || name == MIXIN_CONSTRUCTOR
    def isExceptionResultName(name: Name)   = name startsWith EXCEPTION_RESULT_PREFIX
    def isLocalDummyName(name: Name)        = name startsWith LOCALDUMMY_PREFIX
    def isLocalName(name: Name)             = name endsWith LOCAL_SUFFIX_STRING
    def isLoopHeaderLabel(name: Name)       = (name startsWith WHILE_PREFIX) || (name startsWith DO_WHILE_PREFIX)
    def isProtectedAccessorName(name: Name) = name startsWith PROTECTED_PREFIX
    def isReplWrapperName(name: Name)       = (name containsName INTERPRETER_WRAPPER) || (name containsName INTERPRETER_IMPORT_WRAPPER)
    def isSetterName(name: Name)            = name endsWith SETTER_SUFFIX
    def isTraitSetterName(name: Name)       = isSetterName(name) && (name containsName TRAIT_SETTER_SEPARATOR_STRING)
    def isSingletonName(name: Name)         = name endsWith SINGLETON_SUFFIX
    def isModuleName(name: Name)            = name endsWith MODULE_SUFFIX_NAME
    def isFreshTermName(name: Name)         = name.startsWith(FRESH_TERM_NAME_PREFIX)

    /** Is name a variable name? */
    def isVariableName(name: Name): Boolean = {
      import Character.{isHighSurrogate, isLowSurrogate, isLetter, isLowerCase, isValidCodePoint, toCodePoint}
      val first = name.startChar
      def isLowerLetterSupplementary: Boolean =
        first == '$' && {
          val decoded = name.decoded
          isHighSurrogate(decoded.charAt(0)) && decoded.length > 1 && isLowSurrogate(decoded.charAt(1)) && {
            val codepoint = toCodePoint(decoded.charAt(0), decoded.charAt(1))
            isValidCodePoint(codepoint) && isLetter(codepoint) && isLowerCase(codepoint)
          }
        }
      (    ((first.isLower && first.isLetter) || first == '_' || isLowerLetterSupplementary)
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

    /** Is name a left-associative operator? */
    def isLeftAssoc(operator: Name) = operator.nonEmpty && (operator.endChar != ':')

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
     *  If the name happens to be a back quoted name containing literal $$
     *  or $ followed by an operator that gets encoded, go directly to compiler
     *  crash. Do not pass go and don't even think about collecting any $$
     */
    def unexpandedName(name: Name): Name =
      name.lastIndexOf("$$") match {
        case 0 | -1 => name
        case 1 if name.charAt(0) == '_' => if (name.isTermName) nme.WILDCARD else tpnme.WILDCARD
        case idx0   =>
          // Sketchville - We've found $$ but if it's part of $$$ or $$$$
          // or something we need to keep the bonus dollars, so e.g. foo$$$outer
          // has an original name of $outer.
          var idx = idx0
          while (idx > 0 && name.charAt(idx - 1) == '$')
            idx -= 1
          name.drop(idx + 2)
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
        nameType(DEFAULT_GETTER_INIT_STRING + pos)
      else
        nameType(name.toString + DEFAULT_GETTER_STRING + pos)
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

    def splitDefaultGetterName(name: Name): (Name, Int) = {
      val (n, i) =
        if (name.startsWith(DEFAULT_GETTER_INIT_STRING)) (nme.CONSTRUCTOR, DEFAULT_GETTER_INIT_STRING.length)
        else name.indexOf(DEFAULT_GETTER_STRING) match {
          case -1  => (name.toTermName, -1)
          case idx => (name.toTermName.take(idx), idx + DEFAULT_GETTER_STRING.length)
        }
      if (i < 0) (n, -1)
      else {
        val j = name.indexOf('$', i) // f$default$7$extension
        val idx = name.subSequence(i, if (j < 0) name.length else j)
        (n, idx.toString.toInt)
      }
    }

    def localDummyName(clazz: Symbol): TermName = newTermName(LOCALDUMMY_PREFIX + clazz.name + ">")
    def superName(name: Name, mix: Name = EMPTY): TermName = newTermName(s"${SUPER_PREFIX_STRING}${name}${if (mix.isEmpty) "" else s"$$$mix"}")

    /** The name of an accessor for protected symbols. */
    def protName(name: Name): TermName = newTermName(PROTECTED_PREFIX + name)

    /** The name of a setter for protected symbols. Used for inherited Java fields. */
    def protSetterName(name: Name): TermName = newTermName(PROTECTED_SET_PREFIX + name)

    private[this] val existentialNames = (0 to 22).map(existentialName0)
    private def existentialName0(i: Int) = newTypeName("_" + i)
    final def existentialName(i: Int): TypeName = if (i < existentialNames.length) existentialNames(i) else existentialName0(i)

    final val Nil: NameType    = nameType("Nil")
    final val Predef: NameType = nameType("Predef")

    val _1 : NameType  = nameType("_1")
    val _2 : NameType  = nameType("_2")
    val _3 : NameType  = nameType("_3")
    val _4 : NameType  = nameType("_4")
    val _5 : NameType  = nameType("_5")
    val _6 : NameType  = nameType("_6")
    val _7 : NameType  = nameType("_7")
    val _8 : NameType  = nameType("_8")
    val _9 : NameType  = nameType("_9")
    val _10 : NameType = nameType("_10")
    val _11 : NameType = nameType("_11")
    val _12 : NameType = nameType("_12")
    val _13 : NameType = nameType("_13")
    val _14 : NameType = nameType("_14")
    val _15 : NameType = nameType("_15")
    val _16 : NameType = nameType("_16")
    val _17 : NameType = nameType("_17")
    val _18 : NameType = nameType("_18")
    val _19 : NameType = nameType("_19")
    val _20 : NameType = nameType("_20")
    val _21 : NameType = nameType("_21")
    val _22 : NameType = nameType("_22")

    val x_0 : NameType  = nameType("x$0")
    val x_1 : NameType  = nameType("x$1")
    val x_2 : NameType  = nameType("x$2")
    val x_3 : NameType  = nameType("x$3")
    val x_4 : NameType  = nameType("x$4")
    val x_5 : NameType  = nameType("x$5")
    val x_6 : NameType  = nameType("x$6")
    val x_7 : NameType  = nameType("x$7")
    val x_8 : NameType  = nameType("x$8")
    val x_9 : NameType  = nameType("x$9")

    def syntheticParamName(i: Int): TermName = (i: @switch) match {
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
      case _  => newTermName(s"x$$$i")
    }

    def productAccessorName(j: Int): TermName = (j: @switch) match {
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
    val =:= = encode("=:=")
    val <:< = encode("<:<")

    val DummyImplicit: NameType    = nameType("DummyImplicit")

    val wrapRefArray: NameType     = nameType("wrapRefArray")
    val wrapByteArray: NameType    = nameType("wrapByteArray")
    val wrapShortArray: NameType   = nameType("wrapShortArray")
    val wrapCharArray: NameType    = nameType("wrapCharArray")
    val wrapIntArray: NameType     = nameType("wrapIntArray")
    val wrapLongArray: NameType    = nameType("wrapLongArray")
    val wrapFloatArray: NameType   = nameType("wrapFloatArray")
    val wrapDoubleArray: NameType  = nameType("wrapDoubleArray")
    val wrapBooleanArray: NameType = nameType("wrapBooleanArray")
    val wrapUnitArray: NameType    = nameType("wrapUnitArray")
    val genericWrapArray: NameType = nameType("genericWrapArray")

    val copyArrayToImmutableIndexedSeq: NameType = nameType("copyArrayToImmutableIndexedSeq")

    val double2Double: NameType   = nameType("double2Double")
    val float2Float: NameType     = nameType("float2Float")
    val byte2Byte: NameType       = nameType("byte2Byte")
    val short2Short: NameType     = nameType("short2Short")
    val char2Character: NameType  = nameType("char2Character")
    val int2Integer: NameType     = nameType("int2Integer")
    val long2Long: NameType       = nameType("long2Long")
    val boolean2Boolean: NameType = nameType("boolean2Boolean")

    // Scala 3 import syntax
    val as: NameType              = nameType("as")

    // Scala 3 hard keywords
    val `enum`: NameType          = nameType("enum")
    val `export`: NameType        = nameType("export")
    val `given`: NameType         = nameType("given")
    val `then`: NameType          = nameType("then")

    // Scala 3 soft keywords
    val infix: NameType           = nameType("infix")
    val open: NameType            = nameType("open")
    val using: NameType           = nameType("using")

    // Compiler utilized names

    val AnnotatedType: NameType        = nameType("AnnotatedType")
    val Annotation: NameType           = nameType("Annotation")
    val Any: NameType                  = nameType("Any")
    val AnyVal: NameType               = nameType("AnyVal")
    val Apply: NameType                = nameType("Apply")
    val ArrayAnnotArg: NameType        = nameType("ArrayAnnotArg")
    val CaseDef: NameType              = nameType("CaseDef")
    val ClassInfoType: NameType        = nameType("ClassInfoType")
    val ConstantType: NameType         = nameType("ConstantType")
    val EmptyPackage: NameType         = nameType("EmptyPackage")
    val EmptyPackageClass: NameType    = nameType("EmptyPackageClass")
    val ExistentialType: NameType      = nameType("ExistentialType")
    val Flag : NameType                = nameType("Flag")
    val FlagsRepr: NameType            = nameType("FlagsRepr")
    val Ident: NameType                = nameType("Ident")
    val ImplicitParams: NameType       = nameType("ImplicitParams")
    val Import: NameType               = nameType("Import")
    val Literal: NameType              = nameType("Literal")
    val LiteralAnnotArg: NameType      = nameType("LiteralAnnotArg")
    val MethodType: NameType           = nameType("MethodType")
    val Modifiers: NameType            = nameType("Modifiers")
    val NestedAnnotArg: NameType       = nameType("NestedAnnotArg")
    val New: NameType                  = nameType("New")
    val NoFlags: NameType              = nameType("NoFlags")
    val NoSymbol: NameType             = nameType("NoSymbol")
    val NoMods: NameType               = nameType("NoMods")
    val Nothing: NameType              = nameType("Nothing")
    val Null: NameType                 = nameType("Null")
    val NullaryMethodType: NameType    = nameType("NullaryMethodType")
    val Object: NameType               = nameType("Object")
    val PolyType: NameType             = nameType("PolyType")
    val RefinedType: NameType          = nameType("RefinedType")
    val RootPackage: NameType          = nameType("RootPackage")
    val RootClass: NameType            = nameType("RootClass")
    val Select: NameType               = nameType("Select")
    val SelectFromTypeTree: NameType   = nameType("SelectFromTypeTree")
    val SingleType: NameType           = nameType("SingleType")
    val SuperType: NameType            = nameType("SuperType")
    val This: NameType                 = nameType("This")
    val ThisType: NameType             = nameType("ThisType")
    val Tuple2: NameType               = nameType("Tuple2")
    val TYPE_ : NameType               = nameType("TYPE")
    val TypeBounds: NameType           = nameType("TypeBounds")
    val TypeRef: NameType              = nameType("TypeRef")
    val TypeTree: NameType             = nameType("TypeTree")
    val UNIT : NameType                = nameType("UNIT")
    val accessor: NameType             = nameType("accessor")
    val add_ : NameType                = nameType("add")
    val annotation: NameType           = nameType("annotation")
    val any2stringadd: NameType        = nameType("any2stringadd")
    val anyHash: NameType              = nameType("anyHash")
    val anyValClass: NameType          = nameType("anyValClass")
    val apply: NameType                = nameType("apply")
    val applyDynamic: NameType         = nameType("applyDynamic")
    val applyDynamicNamed: NameType    = nameType("applyDynamicNamed")
    val applyOrElse: NameType          = nameType("applyOrElse")
    val args : NameType                = nameType("args")
    val arrayClass: NameType           = nameType("arrayClass")
    val array_apply : NameType         = nameType("array_apply")
    val array_clone : NameType         = nameType("array_clone")
    val array_length : NameType        = nameType("array_length")
    val array_update : NameType        = nameType("array_update")
    val asModule: NameType             = nameType("asModule")
    val asType: NameType               = nameType("asType")
    val asInstanceOf_ : NameType       = nameType("asInstanceOf")
    val asInstanceOf_Ob : NameType     = nameType(s"$$asInstanceOf") // looks like missing interpolator due to Any member in scope
    val async : NameType               = nameType("async")
    val await : NameType               = nameType("await")
    val box: NameType                  = nameType("box")
    val byteValue: NameType            = nameType("byteValue")
    val bytes: NameType                = nameType("bytes")
    val c: NameType                    = nameType("c")
    val canEqual_ : NameType           = nameType("canEqual")
    val classOf: NameType              = nameType("classOf")
    val clone_ : NameType              = nameType("clone")
    val collection: NameType           = nameType("collection")
    val conforms: NameType             = nameType(s"$$conforms") // $ prefix to avoid shadowing Predef.conforms
    val copy: NameType                 = nameType("copy")
    val create: NameType               = nameType("create")
    val currentMirror: NameType        = nameType("currentMirror")
    val delayedInit: NameType          = nameType("delayedInit")
    val delayedInitArg: NameType       = nameType("delayedInit$body")
    val dollarScope: NameType          = nameType("$scope")
    val doubleHash: NameType           = nameType("doubleHash")
    val doubleValue: NameType          = nameType("doubleValue")
    val drop: NameType                 = nameType("drop")
    val elem: NameType                 = nameType("elem")
    val noSelfType: NameType           = nameType("noSelfType")
    val empty: NameType                = nameType("empty")
    val ensureAccessible : NameType    = nameType("ensureAccessible")
    val eq: NameType                   = nameType("eq")
    val equalsNumChar : NameType       = nameType("equalsNumChar")
    val equalsNumNum : NameType        = nameType("equalsNumNum")
    val equalsNumObject : NameType     = nameType("equalsNumObject")
    val equals_ : NameType             = nameType("equals")
    val error: NameType                = nameType("error")
    val ex: NameType                   = nameType("ex")
    val experimental: NameType         = nameType("experimental")
    val f: NameType                    = nameType("f")
    val false_ : NameType              = nameType("false")
    val filter: NameType               = nameType("filter")
    val finalize_ : NameType           = nameType("finalize")
    val find_ : NameType               = nameType("find")
    val flatMap: NameType              = nameType("flatMap")
    val floatHash: NameType            = nameType("floatHash")
    val floatValue: NameType           = nameType("floatValue")
    val foreach: NameType              = nameType("foreach")
    val freshTermName: NameType        = nameType("freshTermName")
    val freshTypeName: NameType        = nameType("freshTypeName")
    val get: NameType                  = nameType("get")
    val hashCode_ : NameType           = nameType("hashCode")
    val head : NameType                = nameType("head")
    val immutable: NameType            = nameType("immutable")
    val implicitly: NameType           = nameType("implicitly")
    val in: NameType                   = nameType("in")
    val initialize : NameType          = nameType("initialize")
    val initialized : NameType         = nameType("initialized")
    val internal: NameType             = nameType("internal")
    val inlinedEquals: NameType        = nameType("inlinedEquals")
    val intValue: NameType             = nameType("intValue")
    val ioobe : NameType               = nameType("ioobe")
    val isArray: NameType              = nameType("isArray")
    val isDefinedAt: NameType          = nameType("isDefinedAt")
    val isEmpty: NameType              = nameType("isEmpty")
    val isInfinite: NameType           = nameType("isInfinite")
    val isInstanceOf_ : NameType       = nameType("isInstanceOf")
    val isInstanceOf_Ob : NameType     = nameType(s"$$isInstanceOf") // looks like missing interpolator due to Any member in scope
    val isNaN: NameType                = nameType("isNaN")
    val java: NameType                 = nameType("java")
    val key: NameType                  = nameType("key")
    val lang: NameType                 = nameType("lang")
    val length: NameType               = nameType("length")
    val lengthCompare: NameType        = nameType("lengthCompare")
    val locally: NameType              = nameType("locally")
    val longHash: NameType             = nameType("longHash")
    val longValue: NameType            = nameType("longValue")
    val macroContext : NameType        = nameType("c")
    val main: NameType                 = nameType("main")
    val manifestToTypeTag: NameType    = nameType("manifestToTypeTag")
    val map: NameType                  = nameType("map")
    val materializeClassTag: NameType  = nameType("materializeClassTag")
    val materializeWeakTypeTag: NameType = nameType("materializeWeakTypeTag")
    val materializeTypeTag: NameType   = nameType("materializeTypeTag")
    val moduleClass : NameType         = nameType("moduleClass")
    val mkAnnotation: NameType         = nameType("mkAnnotation")
    val mkEarlyDef: NameType           = nameType("mkEarlyDef")
    val mkIdent: NameType              = nameType("mkIdent")
    val mkPackageStat: NameType        = nameType("mkPackageStat")
    val mkRefineStat: NameType         = nameType("mkRefineStat")
    val mkRefTree: NameType            = nameType("mkRefTree")
    val mkSelect: NameType             = nameType("mkSelect")
    val mkThis: NameType               = nameType("mkThis")
    val mkTypeTree: NameType           = nameType("mkTypeTree")
    val ne: NameType                   = nameType("ne")
    val newArray: NameType             = nameType("newArray")
    val newFreeTerm: NameType          = nameType("newFreeTerm")
    val newFreeType: NameType          = nameType("newFreeType")
    val newNestedSymbol: NameType      = nameType("newNestedSymbol")
    val newScopeWith: NameType         = nameType("newScopeWith")
    val notifyAll_ : NameType          = nameType("notifyAll")
    val notify_ : NameType             = nameType("notify")
    val null_ : NameType               = nameType("null")
    val parameterTypes: NameType       = nameType("parameterTypes")
    val pendingSuperCall: NameType     = nameType("pendingSuperCall")
    val prefix : NameType              = nameType("prefix")
    val productArity: NameType         = nameType("productArity")
    val productElement: NameType       = nameType("productElement")
    val productElementName: NameType   = nameType("productElementName")
    val productIterator: NameType      = nameType("productIterator")
    val productPrefix: NameType        = nameType("productPrefix")
    val raw_ : NameType                = nameType("raw")
    val readResolve: NameType          = nameType("readResolve")
    val releaseFence: NameType         = nameType("releaseFence")
    val refl: NameType                 = nameType("refl")
    val reify : NameType               = nameType("reify")
    val reificationSupport : NameType  = nameType("reificationSupport")
    val rootMirror : NameType          = nameType("rootMirror")
    val runtime: NameType              = nameType("runtime")
    val runtimeClass: NameType         = nameType("runtimeClass")
    val runtimeMirror: NameType        = nameType("runtimeMirror")
    val s: NameType                    = nameType("s")
    val scala_ : NameType              = nameType("scala")
    val selectDynamic: NameType        = nameType("selectDynamic")
    val selectOverloadedMethod: NameType = nameType("selectOverloadedMethod")
    val selectTerm: NameType           = nameType("selectTerm")
    val selectType: NameType           = nameType("selectType")
    val self: NameType                 = nameType("self")
    val setAnnotations: NameType       = nameType("setAnnotations")
    val setInfo: NameType              = nameType("setInfo")
    val setSymbol: NameType            = nameType("setSymbol")
    val setType: NameType              = nameType("setType")
    val shortValue: NameType           = nameType("shortValue")
    val splice: NameType               = nameType("splice")
    val staticClass : NameType         = nameType("staticClass")
    val staticModule : NameType        = nameType("staticModule")
    val staticPackage : NameType       = nameType("staticPackage")
    val synchronized_ : NameType       = nameType("synchronized")
    val ScalaDot: NameType             = nameType("ScalaDot")
    val TermName: NameType             = nameType("TermName")
    val this_ : NameType               = nameType("this")
    val thisPrefix : NameType          = nameType("thisPrefix")
    val toArray: NameType              = nameType("toArray")
    val toList: NameType               = nameType("toList")
    val toObjectArray : NameType       = nameType("toObjectArray")
    val toSeq: NameType                = nameType("toSeq")
    val toStats: NameType              = nameType("toStats")
    val TopScope: NameType             = nameType("TopScope")
    val toString_ : NameType           = nameType("toString")
    val toTypeConstructor: NameType    = nameType("toTypeConstructor")
    val tpe : NameType                 = nameType("tpe")
    val tree : NameType                = nameType("tree")
    val true_ : NameType               = nameType("true")
    val tupled: NameType               = nameType("tupled")
    val typedProductIterator: NameType = nameType("typedProductIterator")
    val TypeName: NameType             = nameType("TypeName")
    val typeTagToManifest: NameType    = nameType("typeTagToManifest")
    val unapply: NameType              = nameType("unapply")
    val unapplySeq: NameType           = nameType("unapplySeq")
    val unbox: NameType                = nameType("unbox")
    val unit: NameType                 = nameType("unit")
    val universe: NameType             = nameType("universe")
    val UnliftListElementwise: NameType =nameType( "UnliftListElementwise")
    val UnliftListOfListsElementwise: NameType = nameType("UnliftListOfListsElementwise")
    val update: NameType               = nameType("update")
    val updateDynamic: NameType        = nameType("updateDynamic")
    val value: NameType                = nameType("value")
    val valueOf : NameType             = nameType("valueOf")
    val values : NameType              = nameType("values")
    val wait_ : NameType               = nameType("wait")
    val withFilter: NameType           = nameType("withFilter")
    val writeReplace: NameType         = nameType("writeReplace")
    val xml: NameType                  = nameType("xml")
    val zero: NameType                 = nameType("zero")

    // async
    val result           : NameType       = nameType(s"result$$async") // avoid missing interpolator warnings
    val awaitable        : NameType       = nameType(s"awaitable$$async")
    val completed        : NameType       = nameType(s"completed$$async")
    val stateMachine     : NameType       = nameType(s"stateMachine$$async")
    val state            : NameType       = nameType("state")
    val tr               : NameType       = nameType(s"tr$$async")
    val t                : NameType       = nameType(s"throwable$$async")
    val trGetResult      : NameType       = nameType(s"tryGetResult$$async")

    // quasiquote interpolators:
    val q: NameType  = nameType("q")
    val tq: NameType = nameType("tq")
    val cq: NameType = nameType("cq")
    val pq: NameType = nameType("pq")
    val fq: NameType = nameType("fq")

    // quasiquote's syntactic combinators
    val SyntacticAnnotatedType: NameType    = nameType("SyntacticAnnotatedType")
    val SyntacticApplied: NameType          = nameType("SyntacticApplied")
    val SyntacticAppliedType: NameType      = nameType("SyntacticAppliedType")
    val SyntacticAssign: NameType           = nameType("SyntacticAssign")
    val SyntacticBlock: NameType            = nameType("SyntacticBlock")
    val SyntacticClassDef: NameType         = nameType("SyntacticClassDef")
    val SyntacticCompoundType: NameType     = nameType("SyntacticCompoundType")
    val SyntacticDefDef: NameType           = nameType("SyntacticDefDef")
    val SyntacticEmptyTypeTree: NameType    = nameType("SyntacticEmptyTypeTree")
    val SyntacticExistentialType: NameType  = nameType("SyntacticExistentialType")
    val SyntacticFilter: NameType           = nameType("SyntacticFilter")
    val SyntacticFor: NameType              = nameType("SyntacticFor")
    val SyntacticForYield: NameType         = nameType("SyntacticForYield")
    val SyntacticFunction: NameType         = nameType("SyntacticFunction")
    val SyntacticFunctionType: NameType     = nameType("SyntacticFunctionType")
    val SyntacticImport: NameType           = nameType("SyntacticImport")
    val SyntacticMatch: NameType            = nameType("SyntacticMatch")
    val SyntacticNew: NameType              = nameType("SyntacticNew")
    val SyntacticObjectDef: NameType        = nameType("SyntacticObjectDef")
    val SyntacticPackageObjectDef: NameType = nameType("SyntacticPackageObjectDef")
    val SyntacticPartialFunction: NameType  = nameType("SyntacticPartialFunction")
    val SyntacticPatDef: NameType           = nameType("SyntacticPatDef")
    val SyntacticSelectTerm: NameType       = nameType("SyntacticSelectTerm")
    val SyntacticSelectType: NameType       = nameType("SyntacticSelectType")
    val SyntacticSingletonType: NameType    = nameType("SyntacticSingletonType")
    val SyntacticTermIdent: NameType        = nameType("SyntacticTermIdent")
    val SyntacticTraitDef: NameType         = nameType("SyntacticTraitDef")
    val SyntacticTry: NameType              = nameType("SyntacticTry")
    val SyntacticTuple: NameType            = nameType("SyntacticTuple")
    val SyntacticTupleType: NameType        = nameType("SyntacticTupleType")
    val SyntacticTypeApplied: NameType      = nameType("SyntacticTypeApplied")
    val SyntacticTypeIdent: NameType        = nameType("SyntacticTypeIdent")
    val SyntacticTypeProjection: NameType   = nameType("SyntacticTypeProjection")
    val SyntacticValDef: NameType           = nameType("SyntacticValDef")
    val SyntacticValEq: NameType            = nameType("SyntacticValEq")
    val SyntacticValFrom: NameType          = nameType("SyntacticValFrom")
    val SyntacticVarDef: NameType           = nameType("SyntacticVarDef")

    // unencoded operators
    object raw {
      final val BANG : NameType  = nameType("!")
      final val BAR  : NameType  = nameType("|")
      final val DOLLAR: NameType = nameType("$")
      final val GE: NameType     = nameType(">=")
      final val LE: NameType     = nameType("<=")
      final val MINUS: NameType  = nameType("-")
      final val NE: NameType     = nameType("!=")
      final val PLUS : NameType  = nameType("+")
      final val STAR : NameType  = nameType("*")
      final val TILDE: NameType  = nameType("~")
      final val QMARK: NameType  = nameType("?")

      final val isUnary: Set[Name] = Set(MINUS, PLUS, TILDE, BANG)
    }

    // value-conversion methods
    val toByte: NameType   = nameType("toByte")
    val toShort: NameType  = nameType("toShort")
    val toChar: NameType   = nameType("toChar")
    val toInt: NameType    = nameType("toInt")
    val toLong: NameType   = nameType("toLong")
    val toFloat: NameType  = nameType("toFloat")
    val toDouble: NameType = nameType("toDouble")

    // primitive operation methods for structural types mostly
    // overlap with the above, but not for these two.
    val toCharacter: NameType = nameType("toCharacter")
    val toInteger: NameType   = nameType("toInteger")

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

    val isEncodedUnary = Set[Name](UNARY_~, UNARY_+, UNARY_-, UNARY_!)

    // Grouped here so Cleanup knows what tests to perform.
    val CommonOpNames   = Set[Name](OR, XOR, AND, EQ, NE)
    val BooleanOpNames  = Set[Name](ZOR, ZAND, UNARY_!) ++ CommonOpNames

    val add: NameType                    = nameType("add")
    val complement: NameType             = nameType("complement")
    val divide: NameType                 = nameType("divide")
    val multiply: NameType               = nameType("multiply")
    val negate: NameType                 = nameType("negate")
    val positive: NameType               = nameType("positive")
    val shiftLogicalRight: NameType      = nameType("shiftLogicalRight")
    val shiftSignedLeft: NameType        = nameType("shiftSignedLeft")
    val shiftSignedRight: NameType       = nameType("shiftSignedRight")
    val subtract: NameType               = nameType("subtract")
    val takeAnd: NameType                = nameType("takeAnd")
    val takeConditionalAnd: NameType     = nameType("takeConditionalAnd")
    val takeConditionalOr: NameType      = nameType("takeConditionalOr")
    val takeModulo: NameType             = nameType("takeModulo")
    val takeNot: NameType                = nameType("takeNot")
    val takeOr: NameType                 = nameType("takeOr")
    val takeXor: NameType                = nameType("takeXor")
    val testEqual: NameType              = nameType("testEqual")
    val testGreaterOrEqualThan: NameType = nameType("testGreaterOrEqualThan")
    val testGreaterThan: NameType        = nameType("testGreaterThan")
    val testLessOrEqualThan: NameType    = nameType("testLessOrEqualThan")
    val testLessThan: NameType           = nameType("testLessThan")
    val testNotEqual: NameType           = nameType("testNotEqual")

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
        case ok_name => ok_name
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
    def isTransientBitmap(name: Name) = name == nme.BITMAP_TRANSIENT || name == nme.BITMAP_CHECKINIT_TRANSIENT

    val BITMAP_NORMAL: NameType              = nameType(BITMAP_PREFIX + "")           // initialization bitmap for public/protected lazy vals
    val BITMAP_TRANSIENT: NameType           = nameType(BITMAP_PREFIX + "trans$")     // initialization bitmap for transient lazy vals
    val BITMAP_CHECKINIT: NameType           = nameType(BITMAP_PREFIX + "init$")      // initialization bitmap for checkinit values
    val BITMAP_CHECKINIT_TRANSIENT: NameType = nameType(BITMAP_PREFIX + "inittrans$") // initialization bitmap for transient checkinit values
  }

  lazy val typeNames: tpnme.type = tpnme

  object tpnme extends TypeNames { }

  /** For fully qualified type names.
   */
  object fulltpnme extends TypeNames {
    val RuntimeNothing: NameType = nameType("scala.runtime.Nothing$")
    val RuntimeNull: NameType    = nameType("scala.runtime.Null$")
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

    val isBoxedNumberOrBoolean: NameType = nameType("isBoxedNumberOrBoolean")
    val isBoxedNumber: NameType          = nameType("isBoxedNumber")

    val reflPolyCacheName: NameType   = nameType("reflPoly$Cache")
    val reflParamsCacheName: NameType = nameType("reflParams$Cache")
    val reflMethodName: NameType      = nameType("reflMethod$Method")
    val argument: NameType            = nameType("<argument>")

  }

  class JavaKeywords {
    private[this] val kw = new KeywordSetBuilder

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
    final val FALSEkw: TermName        = kw("false")
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
    final val TRUEkw: TermName         = kw("true")
    final val TRYkw: TermName          = kw("try")
    final val VOIDkw: TermName         = kw("void")
    final val VOLATILEkw: TermName     = kw("volatile")
    final val WHILEkw: TermName        = kw("while")

    final val keywords = kw.result
  }

  // The identifiers non-sealed, permits, record, sealed, var, and yield are restricted identifiers
  // because they are not allowed in some contexts.
  // A type identifier is an identifier that is not the character sequence permits, record, sealed, var, or yield.
  // An unqualified method identifier is an identifier that is not the character sequence yield. (JLS 3.8)
  class JavaRestrictedIdentifiers {
    final val PERMITS: TermName = TermName("permits")
    final val RECORD: TermName = TermName("record")
    final val SEALED: TermName = TermName("sealed")
    final val UNSEALED: TermName = TermName("non-sealed")
    final val NON: TermName = TermName("non")
    final val VAR: TermName    = TermName("var")
    final val YIELD: TermName  = TermName("yield")
  }

  sealed abstract class SymbolNames {
    protected def nameType(s: String): TypeName = newTypeNameCached(s)

    final val BoxedBoolean: String       = "java.lang.Boolean"
    final val BoxedByte: String          = "java.lang.Byte"
    final val BoxedCharacter: String     = "java.lang.Character"
    final val BoxedDouble: String        = "java.lang.Double"
    final val BoxedFloat: String         = "java.lang.Float"
    final val BoxedInteger: String       = "java.lang.Integer"
    final val BoxedLong: String          = "java.lang.Long"
    final val BoxedNumber: String        = "java.lang.Number"
    final val BoxedShort: String         = "java.lang.Short"

    final val GetCause: TermName         = newTermName("getCause")
    final val GetClass: TermName         = newTermName("getClass")
    final val GetClassLoader: TermName   = newTermName("getClassLoader")
    final val GetMethod: TermName        = newTermName("getMethod")
    final val Invoke: TermName           = newTermName("invoke")
    final val InvokeExact: TermName      = newTermName("invokeExact")

    final val Metafactory: TermName         = newTermName("metafactory")
    final val AltMetafactory: TermName      = newTermName("altMetafactory")
    final val Bootstrap: TermName           = newTermName("bootstrap")

    val Boxed = immutable.Map[TypeName, String](
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
