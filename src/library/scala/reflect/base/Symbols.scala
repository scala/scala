package scala.reflect
package base

trait Symbols { self: Universe =>

  /** The abstract type of symbols representing declarations */
  type Symbol >: Null <: SymbolBase

  /** A tag that preserves the identity of the `Symbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val SymbolTag: ClassTag[Symbol]

  /** The abstract type of type symbols representing type, class, and trait declarations,
   *  as well as type parameters
   */
  type TypeSymbol >: Null <: Symbol with TypeSymbolBase

  /** A tag that preserves the identity of the `TypeSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val TypeSymbolTag: ClassTag[TypeSymbol]

  /** The abstract type of term symbols representing val, var, def, and object declarations as
   *  well as packages and value parameters.
   */
  type TermSymbol >: Null <: Symbol with TermSymbolBase

  /** A tag that preserves the identity of the `TermSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val TermSymbolTag: ClassTag[TermSymbol]

  /** The abstract type of method symbols representing def declarations */
  type MethodSymbol >: Null <: TermSymbol with MethodSymbolBase

  /** A tag that preserves the identity of the `MethodSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val MethodSymbolTag: ClassTag[MethodSymbol]

  /** The abstract type of module symbols representing object declarations */
  type ModuleSymbol >: Null <: TermSymbol with ModuleSymbolBase

  /** A tag that preserves the identity of the `ModuleSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ModuleSymbolTag: ClassTag[ModuleSymbol]

  /** The abstract type of class symbols representing class and trait definitions */
  type ClassSymbol >: Null <: TypeSymbol with ClassSymbolBase

  /** A tag that preserves the identity of the `ClassSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ClassSymbolTag: ClassTag[ClassSymbol]

  /** The abstract type of free terms introduced by reification */
  type FreeTermSymbol >: Null <: TermSymbol with FreeTermSymbolBase

  /** A tag that preserves the identity of the `FreeTermSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val FreeTermSymbolTag: ClassTag[FreeTermSymbol]

  /** The abstract type of free types introduced by reification */
  type FreeTypeSymbol >: Null <: TypeSymbol with FreeTypeSymbolBase

  /** A tag that preserves the identity of the `FreeTypeSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val FreeTypeSymbolTag: ClassTag[FreeTypeSymbol]

  /** A special "missing" symbol */
  val NoSymbol: Symbol

  /** The base API that all symbols support */
  trait SymbolBase { this: Symbol =>

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

    /** The type of the symbol name.
     *  Can be either `TermName` or `TypeName` depending on whether this is a `TermSymbol` or a `TypeSymbol`.
     *
     *  Type name namespaces do not intersect with term name namespaces.
     *  This fact is reflected in different types for names of `TermSymbol` and `TypeSymbol`.
     */
    type NameType >: Null <: Name

    /** The name of the symbol as a member of the `Name` type.
     */
    def name: Name

    /** The encoded full path name of this symbol, where outer names and inner names
     *  are separated by periods.
     */
    def fullName: String

    /** Does this symbol represent the definition of a type?
     *  Note that every symbol is either a term or a type.
     *  So for every symbol `sym` (except for `NoSymbol`),
     *  either `sym.isTerm` is true or `sym.isType` is true.
     */
    def isType: Boolean = false

    /** This symbol cast to a TypeSymbol.
     *  @throws ScalaReflectionException if `isType` is false.
     */
    def asType: TypeSymbol = throw new ScalaReflectionException(s"$this is not a type")

    /** Does this symbol represent the definition of a term?
     *  Note that every symbol is either a term or a type.
     *  So for every symbol `sym` (except for `NoSymbol`),
     *  either `sym.isTerm` is true or `sym.isTerm` is true.
     */
    def isTerm: Boolean = false

    /** This symbol cast to a TermSymbol.
     *  @throws ScalaReflectionException if `isTerm` is false.
     */
    def asTerm: TermSymbol = throw new ScalaReflectionException(s"$this is not a term")

    /** Does this symbol represent the definition of a method?
     *  If yes, `isTerm` is also guaranteed to be true.
     */
    def isMethod: Boolean = false

    /** This symbol cast to a MethodSymbol.
     *  @throws ScalaReflectionException if `isMethod` is false.
     */
    def asMethod: MethodSymbol = {
      def overloadedMsg =
        "encapsulates multiple overloaded alternatives and cannot be treated as a method. "+
        "Consider invoking `<offending symbol>.asTerm.alternatives` and manually picking the required method"
      def vanillaMsg = "is not a method"
      val msg = if (isOverloadedMethod) overloadedMsg else vanillaMsg
      throw new ScalaReflectionException(s"$this $msg")
    }

    /** Used to provide a better error message for `asMethod` */
    protected def isOverloadedMethod = false

    /** Does this symbol represent the definition of a module (i.e. it
     *  results from an object definition?).
     *  If yes, `isTerm` is also guaranteed to be true.
     */
    def isModule: Boolean = false

    /** This symbol cast to a ModuleSymbol defined by an object definition.
     *  @throws ScalaReflectionException if `isModule` is false.
     */
    def asModule: ModuleSymbol = throw new ScalaReflectionException(s"$this is not a module")

    /** Does this symbol represent the definition of a class or trait?
     *  If yes, `isType` is also guaranteed to be true.
     */
    def isClass: Boolean = false

    /** Does this symbol represent the definition of a class implicitly associated
     *  with an object definition (module class in scala compiler parlance).
     *  If yes, `isType` is also guaranteed to be true.
     */
    def isModuleClass: Boolean = false

    /** This symbol cast to a ClassSymbol representing a class or trait.
     *  @throws ScalaReflectionException if `isClass` is false.
     */
    def asClass: ClassSymbol = throw new ScalaReflectionException(s"$this is not a class")

    /** Does this symbol represent a free term captured by reification?
     *  If yes, `isTerm` is also guaranteed to be true.
     */
    def isFreeTerm: Boolean = false

    /** This symbol cast to a free term symbol.
     *  @throws ScalaReflectionException if `isFreeTerm` is false.
     */
    def asFreeTerm: FreeTermSymbol = throw new ScalaReflectionException(s"$this is not a free term")

    /** Does this symbol represent a free type captured by reification?
     *  If yes, `isType` is also guaranteed to be true.
     */
    def isFreeType: Boolean = false

    /** This symbol cast to a free type symbol.
     *  @throws ScalaReflectionException if `isFreeType` is false.
     */
    def asFreeType: FreeTypeSymbol = throw new ScalaReflectionException(s"$this is not a free type")

    def newTermSymbol(name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TermSymbol
    def newModuleAndClassSymbol(name: Name, pos: Position = NoPosition, flags: FlagSet = NoFlags): (ModuleSymbol, ClassSymbol)
    def newMethodSymbol(name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): MethodSymbol
    def newTypeSymbol(name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TypeSymbol
    def newClassSymbol(name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): ClassSymbol
  }

  /** The base API that all type symbols support */
  trait TypeSymbolBase extends SymbolBase { this: TypeSymbol =>
    /** Type symbols have their names of type `TypeName`.
     */
    final type NameType = TypeName

    /** The type constructor corresponding to this type symbol.
     *  This is different from `toType` in that type parameters
     *  are part of results of `toType`, but not of `toTypeConstructor`.
     *
     *  Example: Given a class declaration `class C[T] { ... } `, that generates a symbol
     *  `C`. Then `C.toType` is the type `C[T]`, but `C.toTypeConstructor` is `C`.
     */
    def toTypeConstructor: Type

    /** A type reference that refers to this type symbol seen
     *  as a member of given type `site`.
     */
    def toTypeIn(site: Type): Type

    /**  A type reference that refers to this type symbol
      *  Note if symbol is a member of a class, one almost always is interested
      *  in `asTypeIn` with a site type instead.
      *
      *  Example: Given a class declaration `class C[T] { ... } `, that generates a symbol
      *  `C`. Then `C.toType` is the type `C[T]`.
      *
      *  By contrast, `C.typeSignature` would be a type signature of form
      *  `PolyType(ClassInfoType(...))` that describes type parameters, value
      *  parameters, parent types, and members of `C`.
      */
    def toType: Type

    final override def isType = true
    final override def asType = this
  }

  /** The base API that all term symbols support */
  trait TermSymbolBase extends SymbolBase { this: TermSymbol =>
    /** Term symbols have their names of type `TermName`.
     */
    final type NameType = TermName

    final override def isTerm = true
    final override def asTerm = this
  }

  /** The base API that all method symbols support */
  trait MethodSymbolBase extends TermSymbolBase { this: MethodSymbol =>
    final override def isMethod = true
    final override def asMethod = this
  }

  /** The base API that all module symbols support */
  trait ModuleSymbolBase extends TermSymbolBase { this: ModuleSymbol =>
    /** The class implicitly associated with the object definition.
     *  One can go back from a module class to the associated module symbol
     *  by inspecting its `selfType.termSymbol`.
     */
    def moduleClass: Symbol // needed for tree traversals
    // when this becomes `moduleClass: ClassSymbol`, it will be the happiest day in my life

    final override def isModule = true
    final override def asModule = this
  }

  /** The base API that all class symbols support */
  trait ClassSymbolBase extends TypeSymbolBase { this: ClassSymbol =>
    final override def isClass = true
    final override def asClass = this
  }

  /** The base API that all free type symbols support */
  trait FreeTypeSymbolBase extends TypeSymbolBase { this: FreeTypeSymbol =>
    final override def isFreeType = true
    final override def asFreeType = this
  }

  /** The base API that all free term symbols support */
  trait FreeTermSymbolBase extends TermSymbolBase { this: FreeTermSymbol =>
    final override def isFreeTerm = true
    final override def asFreeTerm = this
  }
}
