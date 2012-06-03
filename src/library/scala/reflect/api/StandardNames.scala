/* NSC -- new Scala compiler
* Copyright 2005-2011 LAMP/EPFL
* @author  Martin Odersky
*/
package scala.reflect
package api

trait StandardNames extends base.StandardNames {
  self: Universe =>

  val nme: TermNamesApi
  val tpnme: TypeNamesApi

  trait NamesApi extends NamesBase {
    val ANON_CLASS_NAME: NameType
    val ANON_FUN_NAME: NameType
    val EMPTY: NameType
    val ERROR: NameType
    val IMPORT: NameType
    val MODULE_VAR_SUFFIX: NameType
    val PACKAGE: NameType
    val ROOT: NameType
    val SPECIALIZED_SUFFIX: NameType

    def flattenedName(segments: Name*): NameType
  }

  trait TermNamesApi extends NamesApi with TermNamesBase {
    val EXPAND_SEPARATOR_STRING: String
    val IMPL_CLASS_SUFFIX: String
    val INTERPRETER_IMPORT_WRAPPER: String
    val INTERPRETER_LINE_PREFIX: String
    val INTERPRETER_VAR_PREFIX: String
    val INTERPRETER_WRAPPER_SUFFIX: String
    val LOCALDUMMY_PREFIX: String
    val LOCAL_SUFFIX_STRING: String
    val MODULE_SUFFIX_NAME: TermName
    val NAME_JOIN_NAME: TermName
    val PROTECTED_PREFIX: String
    val PROTECTED_SET_PREFIX: String
    val SETTER_SUFFIX: TermName
    val SINGLETON_SUFFIX: String
    val SUPER_PREFIX_STRING: String
    val TRAIT_SETTER_SEPARATOR_STRING: String

    val ANYNAME: TermName
    val FAKE_LOCAL_THIS: TermName
    val INITIALIZER: TermName
    val LAZY_LOCAL: TermName
    val UNIVERSE_BUILD: NameType
    val UNIVERSE_BUILD_PREFIX: NameType
    val UNIVERSE_PREFIX: NameType
    val UNIVERSE_SHORT: NameType
    val MIRROR_PREFIX: NameType
    val MIRROR_SHORT: NameType
    val MIRROR_UNTYPED: NameType
    val REIFY_FREE_PREFIX: NameType
    val REIFY_FREE_THIS_SUFFIX: NameType
    val REIFY_FREE_VALUE_SUFFIX: NameType
    val REIFY_SYMDEF_PREFIX: NameType
    val MIXIN_CONSTRUCTOR: TermName
    val MODULE_INSTANCE_FIELD: TermName
    val OUTER: TermName
    val OUTER_LOCAL: TermName
    val OUTER_SYNTH: TermName
    val SELECTOR_DUMMY: TermName
    val SELF: TermName
    val SPECIALIZED_INSTANCE: TermName
    val STAR: TermName
    val THIS: TermName

    val BITMAP_NORMAL: TermName
    val BITMAP_TRANSIENT: TermName
    val BITMAP_CHECKINIT: TermName
    val BITMAP_CHECKINIT_TRANSIENT: TermName

    val ROOTPKG: TermName

    val ADD: TermName
    val AND: TermName
    val ASR: TermName
    val DIV: TermName
    val EQ: TermName
    val EQL: TermName
    val GE: TermName
    val GT: TermName
    val HASHHASH: TermName
    val LE: TermName
    val LSL: TermName
    val LSR: TermName
    val LT: TermName
    val MINUS: TermName
    val MOD: TermName
    val MUL: TermName
    val NE: TermName
    val OR: TermName
    val PLUS : TermName
    val SUB: TermName
    val XOR: TermName
    val ZAND: TermName
    val ZOR: TermName

    val UNARY_~ : TermName
    val UNARY_+ : TermName
    val UNARY_- : TermName
    val UNARY_! : TermName

    val ??? : TermName

    def isConstructorName(name: Name): Boolean
    def isExceptionResultName(name: Name): Boolean
    def isImplClassName(name: Name): Boolean
    def isLocalDummyName(name: Name): Boolean
    def isLocalName(name: Name): Boolean
    def isLoopHeaderLabel(name: Name): Boolean
    def isModuleName(name: Name): Boolean
    def isOpAssignmentName(name: Name): Boolean
    def isProtectedAccessorName(name: Name): Boolean
    def isReplWrapperName(name: Name): Boolean
    def isSetterName(name: Name): Boolean
    def isSingletonName(name: Name): Boolean
    def isSuperAccessorName(name: Name): Boolean
    def isTraitSetterName(name: Name): Boolean

    def defaultGetterName(name: Name, pos: Int): TermName
    def defaultGetterToMethod(name: Name): TermName
    def expandedName(name: TermName, base: Symbol, separator: String): TermName
    def expandedSetterName(name: TermName, base: Symbol): TermName
    def getterName(name: TermName): TermName
    def getterToLocal(name: TermName): TermName
    def getterToSetter(name: TermName): TermName
    def localDummyName(clazz: Symbol): TermName
    def localToGetter(name: TermName): TermName
    def protName(name: Name): TermName
    def protSetterName(name: Name): TermName
    def setterToGetter(name: TermName): TermName
    def superName(name: Name): TermName

    def dropLocalSuffix(name: Name): Name
    def originalName(name: Name): Name
    def stripModuleSuffix(name: Name): Name
    def unspecializedName(name: Name): Name
    def segments(name: String, assumeTerm: Boolean): List[Name]
    def splitSpecializedName(name: Name): (Name, String, String)
  }

  trait TypeNamesApi extends NamesApi with TypeNamesBase {
    val BYNAME_PARAM_CLASS_NAME: TypeName
    val EQUALS_PATTERN_NAME: TypeName
    val JAVA_REPEATED_PARAM_CLASS_NAME: TypeName
    val LOCAL_CHILD: TypeName
    val REFINE_CLASS_NAME: TypeName
    val REPEATED_PARAM_CLASS_NAME: TypeName
    val WILDCARD_STAR: TypeName
    val REIFY_TYPECREATOR_PREFIX: NameType
    val REIFY_TREECREATOR_PREFIX: NameType

    def dropSingletonName(name: Name): TypeName
    def implClassName(name: Name): TypeName
    def interfaceName(implname: Name): TypeName
    def singletonName(name: Name): TypeName
  }
}
