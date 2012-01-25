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

    /** The owner of this symbol. This is the symbol
     *  that directly contains the current symbol's definition.
     *  The `NoSymbol` symbol does not have an owner, and calling this method
     *  on one causes an internal error.
     *  The owner of the Scala root class [[scala.reflect.api.mirror.RootClass]]
     *  and the Scala root object [[scala.reflect.api.mirror.RootPackage]] is `NoSymbol`.
     *  Every other symbol has a chain of owners that ends in
     *  [[scala.reflect.api.mirror.RootClass]].
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

    /** An id number which is unique for all symbols in this universe */
    def id: Int

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

    /** For a class: the module or case class factory with the same name in the same package.
     *  For all others: NoSymbol
     */
    def companionModule: Symbol

    /** For a module: the class with the same name in the same package.
     *  For all others: NoSymbol
     */
    def companionClass: Symbol

    /** The module corresponding to this module class (note that this
     *  is not updated when a module is cloned), or NoSymbol if this is not a ModuleClass
     */
    def sourceModule: Symbol

    /** If symbol is an object definition, its implied associated class,
     *  otherwise NoSymbol
     */
    def moduleClass: Symbol // needed for LiftCode

    /** The top-level class containing this symbol. */
    def toplevelClass: Symbol

    /** The next enclosing class, or `NoSymbol` if none exists */
    def enclClass      : Symbol

    /** The next enclosing method, or `NoSymbol` if none exists */
    def enclMethod     : Symbol

    /** Does this symbol represent the definition of term?
     *  Note that every symbol is either a term or a type.
     *  So for every symbol `sym`, either `sym.isTerm` is true
     *  or `sym.isType` is true.
     */
    def isTerm         : Boolean

    /** Does this symbol represent the definition of type?
     *  Note that every symbol is either a term or a type.
     *  So for every symbol `sym`, either `sym.isTerm` is true
     *  or `sym.isType` is true.
     */
    def isType         : Boolean

    /** Does this symbol represent the definition of class?
     *  If yes, `isType` is also guaranteed to be true.
     */
    def isClass        : Boolean

    /** Does this symbol represent the definition of a type alias?
     *  If yes, `isType` is also guaranteed to be true.
     */
    def isAliasType    : Boolean

    /** Does this symbol represent the definition of an abstract type?
     *  If yes, `isType` is also guaranteed to be true.
     */
    def isAbstractType : Boolean

    /** The type signature of this symbol.
     *  Note if the symbol is a member of a class, one almost always is interested
     *  in `typeSigIn` with a site type instead.
     */
    def typeSig: Type

    /** The type signature of this symbol seen as a member of given type `site`.
     */
    def typeSigIn(site: Type): Type

   /**  A type reference that refers to this type symbol
     *  Note if symbol is a member of a class, one almost always is interested
     *  in `asTypeIn` with a site type instead.
     *
     *  Example: Given a class declaration `class C[T] { ... } `, that generates a symbol
     *  `C`. Then `C.asType` is the type `C[T]`.
     *
     *  By contrast, `C.typeSig` would be a type signature of form
     *  `PolyType(ClassInfoType(...))` that describes type parameters, value
     *  parameters, parent types, and members of `C`.
     */
    def asType: Type

    /** A type reference that refers to this type symbol seen
     *  as a member of given type `site`.
     */
    def asTypeIn(site: Type): Type

    /** The type constructor corresponding to this type symbol.
     *  This is different from `asType` in that type parameters
     *  are part of results of `asType`, but not of `asTypeConstructor`.
     *
     *  Example: Given a class declaration `class C[T] { ... } `, that generates a symbol
     *  `C`. Then `C.asType` is the type `C[T]`, but `C.asTypeCponstructor` is `C`.
     */
    def asTypeConstructor: Type  // needed by LiftCode

    /** If this symbol is a class or trait, its self type, otherwise the type
     *  of the symbol itself.
     */
    def typeOfThis: Type

    /** If this symbol is a class, the type `C.this`, otherwise `NoPrefix`.
     */
    def thisType: Type

    /** A fresh symbol with given name `name`, position `pos` and flags `flags` that has
     *  the current symbol as its owner. 
     */
    def newNestedSymbol(name: Name, pos: Position, flags: Long): Symbol // needed by LiftCode

    /** Low-level operation to set the symbol's flags
     *  @return the symbol itself
     */
    def setInternalFlags(flags: Long): this.type // needed by LiftCode

    /** Set symbol's type signature to given type
     *  @return the symbol itself
     */
    def setTypeSig(tpe: Type): this.type // needed by LiftCode

    /** Set symbol's annotations to given annotations `annots`.
     */
    def setAnnotations(annots: AnnotationInfo*): this.type // needed by LiftCode
  }

  val NoSymbol: Symbol
}
