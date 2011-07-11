package scala.reflect
package api

trait Symbols { self: Universe =>

  type Symbol >: Null <: AbsSymbol

  abstract class AbsSymbol { this: Symbol =>

    /** The modifiers of this symbol
     */
    def allModifiers: Set[Modifier.Value]

    /** Does this symbol have given modifier?
     */
    def hasModifier(mod: Modifier.Value): Boolean

    /** The owner of this symbol.
     */
    def owner: Symbol

    /** The name of the symbol as a member of the `Name` type.
     */
    def name: Name

    /** The name of the symbol before decoding, e.g. `\$eq\$eq` instead of `==`.
     */
    def encodedName: String

    /** The decoded name of the symbol, e.g. `==` instead of `\$eq\$eq`.
     */
    def decodedName: String

    /** The encoded full path name of this symbol, where outer names and inner names
     *  are separated by periods.
     */
    def fullName: String

    /**
     * Set when symbol has a modifier of the form private[X], NoSymbol otherwise.
     *
     *  Access level encoding: there are three scala flags (PRIVATE, PROTECTED,
     *  and LOCAL) which combine with value privateWithin (the "foo" in private[foo])
     *  to define from where an entity can be accessed.  The meanings are as follows:
     *
     *  PRIVATE     access restricted to class only.
     *  PROTECTED   access restricted to class and subclasses only.
     *  LOCAL       can only be set in conjunction with PRIVATE or PROTECTED.
     *              Further restricts access to the same object instance.
     *
     *  In addition, privateWithin can be used to set a visibility barrier.
     *  When set, everything contained in the named enclosing package or class
     *  has access.  It is incompatible with PRIVATE or LOCAL, but is additive
     *  with PROTECTED (i.e. if either the flags or privateWithin allow access,
     *  then it is allowed.)
     *
     *  The java access levels translate as follows:
     *
     *  java private:     hasFlag(PRIVATE)                && !hasAccessBoundary
     *  java package:     !hasFlag(PRIVATE | PROTECTED)   && (privateWithin == enclosing package)
     *  java protected:   hasFlag(PROTECTED)              && (privateWithin == enclosing package)
     *  java public:      !hasFlag(PRIVATE | PROTECTED)   && !hasAccessBoundary
     */
    def privateWithin: Symbol

    /** Whether this symbol has a "privateWithin" visibility barrier attached.
     */
    def hasAccessBoundary: Boolean

    /** A list of annotations attached to this Symbol.
     */
    def annotations: List[self.AnnotationInfo]

    /** The type of the symbol
     */
    def tpe: Type

    /** The info of the symbol. This is like tpe, except for class symbols where the `info`
     *  describes the contents of the class whereas the `tpe` is a reference to the class.
     */
    def info: Type

    /** If this symbol is a class or trait, its self type, otherwise the type of the symbol itse;lf
     */
    def typeOfThis: Type

    /** For a module class its linked class, for a plain class
     *  the module class of its linked module.
     *  For instance
     *    object Foo
     *    class Foo
     *
     *  Then object Foo has a `moduleClass` (invisible to the user, the backend calls it Foo$
     *  linkedClassOfClass goes from class Foo$ to class Foo, and back.
     */
    def linkedClassOfClass: Symbol

    /** The module corresponding to this module class (note that this
     *  is not updated when a module is cloned), or NoSymbol if this is not a ModuleClass
     */
    def sourceModule: Symbol

    /** If symbol is an object definition, it's implied associated class,
     *  otherwise NoSymbol
     */
    def moduleClass: Symbol

    /** The top-level class containing this symbol. */
    def toplevelClass: Symbol

    /**
     *  If symbol is a lazy val, it's lazy accessor
     */
    def lazyAccessor: Symbol

    /** The next enclosing class */
    def enclClass      : Symbol

    /** The next enclosing method */
    def enclMethod     : Symbol

    def isTerm         : Boolean
    def isType         : Boolean
    def isClass        : Boolean
    def isAliasType    : Boolean
    def isAbstractType : Boolean

    def isAbstractClass      : Boolean
    def isBridge             : Boolean
    def isConcreteClass      : Boolean
    def isContravariant      : Boolean
    def isCovariant          : Boolean
    def isEarlyInitialized   : Boolean
    def isExistentiallyBound : Boolean
    def isImplClass          : Boolean
    def isLazyAccessor       : Boolean
    def isMethod             : Boolean
    def isModule             : Boolean
    def isModuleClass        : Boolean
    def isNumericValueClass  : Boolean
    def isOverloaded         : Boolean
    def isRefinementClass    : Boolean
    def isSourceMethod       : Boolean
    def isTypeParameter      : Boolean
    def isValueClass         : Boolean
    def isVarargsMethod      : Boolean

    /** Package tests */
    def isPackage      : Boolean
    def isPackageClass : Boolean
    def isRootPackage  : Boolean
    def isRoot         : Boolean
    def isEmptyPackage : Boolean
    def isEmptyPackageClass: Boolean
  }

  val NoSymbol: Symbol
}
