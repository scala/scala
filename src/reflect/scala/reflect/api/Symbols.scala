package scala.reflect
package api

/**
 * Defines the type hierachy for symbols
 *
 * @see [[scala.reflect]] for a description on how the class hierarchy is encoded here.
 */
trait Symbols { self: Universe =>

  /** The type of symbols representing declarations */
  type Symbol >: Null <: SymbolApi

  /** A tag that preserves the identity of the `Symbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val SymbolTag: ClassTag[Symbol]

  /** The type of type symbols representing type, class, and trait declarations,
   *  as well as type parameters
   */
  type TypeSymbol >: Null <: Symbol with TypeSymbolApi

  /** A tag that preserves the identity of the `TypeSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val TypeSymbolTag: ClassTag[TypeSymbol]

  /** The type of term symbols representing val, var, def, and object declarations as
   *  well as packages and value parameters.
   */
  type TermSymbol >: Null <: Symbol with TermSymbolApi

  /** A tag that preserves the identity of the `TermSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val TermSymbolTag: ClassTag[TermSymbol]

  /** The type of method symbols representing def declarations */
  type MethodSymbol >: Null <: TermSymbol with MethodSymbolApi

  /** A tag that preserves the identity of the `MethodSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val MethodSymbolTag: ClassTag[MethodSymbol]

  /** The type of module symbols representing object declarations */
  type ModuleSymbol >: Null <: TermSymbol with ModuleSymbolApi

  /** A tag that preserves the identity of the `ModuleSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ModuleSymbolTag: ClassTag[ModuleSymbol]

  /** The type of class symbols representing class and trait definitions */
  type ClassSymbol >: Null <: TypeSymbol with ClassSymbolApi

  /** A tag that preserves the identity of the `ClassSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ClassSymbolTag: ClassTag[ClassSymbol]

  /** The type of free terms introduced by reification */
  type FreeTermSymbol >: Null <: TermSymbol with FreeTermSymbolApi

  /** A tag that preserves the identity of the `FreeTermSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val FreeTermSymbolTag: ClassTag[FreeTermSymbol]

  /** The type of free types introduced by reification */
  type FreeTypeSymbol >: Null <: TypeSymbol with FreeTypeSymbolApi

  /** A tag that preserves the identity of the `FreeTypeSymbol` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val FreeTypeSymbolTag: ClassTag[FreeTypeSymbol]

  /** A special "missing" symbol */
  val NoSymbol: Symbol

  /** The API of symbols */
  trait SymbolApi { this: Symbol =>

    /** The owner of this symbol. This is the symbol
     *  that directly contains the current symbol's definition.
     *  The `NoSymbol` symbol does not have an owner, and calling this method
     *  on one causes an internal error.
     *  The owner of the Scala root class [[scala.reflect.api.Mirror.RootClass]]
     *  and the Scala root object [[scala.reflect.api.Mirror.RootPackage]] is `NoSymbol`.
     *  Every other symbol has a chain of owners that ends in
     *  [[scala.reflect.api.Mirror.RootClass]].
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

    /** Source file if this symbol is created during this compilation run,
     *  or a class file if this symbol is loaded from a *.class or *.jar.
     *
     *  The return type is [[scala.reflect.io.AbstractFile]], which belongs to an experimental part of Scala reflection.
     *  It should not be used unless you know what you are doing. In subsequent releases, this API will be refined
     *  and exposed as a part of scala.reflect.api.
     */
    def associatedFile: scala.reflect.io.AbstractFile

    /** A list of annotations attached to this Symbol.
     */
    def annotations: List[Annotation]

    /** For a class: the module or case class factory with the same name in the same package.
     *  For a module: the class with the same name in the same package.
     *  For all others: NoSymbol
     */
    def companionSymbol: Symbol

    /** The type signature of this symbol seen as a member of given type `site`.
     */
    def typeSignatureIn(site: Type): Type

    /** The type signature of this symbol.
     *  Note if the symbol is a member of a class, one almost always is interested
     *  in `typeSignatureIn` with a site type instead.
     */
    def typeSignature: Type

    /** Returns all symbols overriden by this symbol. */
    def allOverriddenSymbols: List[Symbol]

    /******************* tests *******************/

    /** Does this symbol represent a synthetic (i.e. a compiler-generated) entity?
     *  Examples of synthetic entities are accessors for vals and vars
     *  or mixin constructors in trait implementation classes.
     */
    def isSynthetic: Boolean

    /** Does this symbol represent an implementation artifact that isn't meant for public use?
     *  Examples of such artifacts are erasure bridges and $outer fields.
     */
    def isImplementationArtifact: Boolean

    /** Does this symbol represent a local declaration or definition?
     *
     *  If yes, either `isPrivate` or `isProtected` are guaranteed to be true.
     *  Local symbols can only be accessed from the same object instance.
     *
     *  If yes, `privateWithin` might tell more about this symbol's visibility scope.
     */
    def isLocal: Boolean

    /** Does this symbol represent a private declaration or definition?
     *  If yes, `privateWithin` might tell more about this symbol's visibility scope.
     */
    def isPrivate: Boolean

    /** Does this symbol represent a protected declaration or definition?
     *  If yes, `privateWithin` might tell more about this symbol's visibility scope.
     */
    def isProtected: Boolean

    /** Does this symbol represent a public declaration or definition?
     */
    def isPublic: Boolean

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
     *  java private:     isPrivate                  && (privateWithin == NoSymbol)
     *  java package:     !isPrivate && !isProtected && (privateWithin == enclosingPackage)
     *  java protected:   isProtected                && (privateWithin == enclosingPackage)
     *  java public:      !isPrivate && !isProtected && (privateWithin == NoSymbol)
     */
    def privateWithin: Symbol

    /** Does this symbol represent the definition of a package?
     *  If yes, `isTerm` is also guaranteed to be true.
     */
    def isPackage: Boolean

    /** Does this symbol represent a package class?
     *  If yes, `isClass` is also guaranteed to be true.
     */
    def isPackageClass: Boolean

    /** Does this symbol or its underlying type represent a typechecking error?
     */
    def isErroneous : Boolean

    /** Is this symbol static (i.e. with no outer instance)?
     *  Q: When exactly is a sym marked as STATIC?
     *  A: If it's a member of a toplevel object, or of an object contained in a toplevel object, or any number of levels deep.
     *  http://groups.google.com/group/scala-internals/browse_thread/thread/d385bcd60b08faf6
     */
    def isStatic: Boolean

    /** Is this symbol final?
     */
    def isFinal: Boolean

    /** Is this symbol overriding something?
     */
    def isOverride: Boolean

    /** Is this symbol labelled as "abstract override"?
     */
    def isAbstractOverride: Boolean

    /** Is this symbol a macro?
     */
    def isMacro: Boolean

    /** Is this symbol a parameter (either a method parameter or a type parameter)?
     */
    def isParameter: Boolean

    /** Is this symbol a specialized type parameter or a generated specialized member?
     */
    def isSpecialized: Boolean

    /** Is this symbol defined by Java?
     */
    def isJava: Boolean

    /** Does this symbol represent an implicit value, definition, class or parameter?
     */
    def isImplicit: Boolean

    /******************* helpers *******************/

    /** ...
     */
    def orElse(alt: => Symbol): Symbol

    /** ...
     */
    def filter(cond: Symbol => Boolean): Symbol

    /** If this is a NoSymbol, returns NoSymbol, otherwise
     *  returns the result of applying `f` to this symbol.
     */
    def map(f: Symbol => Symbol): Symbol

    /** ...
     */
    def suchThat(cond: Symbol => Boolean): Symbol
  }

  /** The API of term symbols */
  trait TermSymbolApi extends SymbolApi { this: TermSymbol =>
    /** Term symbols have their names of type `TermName`.
     */
    final type NameType = TermName

    final override def isTerm = true
    final override def asTerm = this

    /** Is this symbol introduced as `val`?
     */
    def isVal: Boolean

    /** Does this symbol denote a stable value? */
    def isStable: Boolean

    /** Is this symbol introduced as `var`?
     */
    def isVar: Boolean

    /** Does this symbol represent a getter or a setter?
     */
    def isAccessor: Boolean

    /** Does this symbol represent a getter of a field?
     *  If yes, `isMethod` is also guaranteed to be true.
     */
    def isGetter: Boolean

    /** Does this symbol represent a setter of a field?
     *  If yes, `isMethod` is also guaranteed to be true.
     */
    def isSetter: Boolean

    /** Does this symbol represent an overloaded method?
     *  If yes, `isMethod` is false, and the list of the enclosed alternatives can be found out via `alternatives`.
     */
    def isOverloaded   : Boolean

    /** Does this symbol represent a lazy value?
     */
    def isLazy: Boolean

    /** The overloaded alternatives of this symbol */
    def alternatives: List[Symbol]

    /** Used to provide a better error message for `asMethod` */
    override protected def isOverloadedMethod = alternatives exists (_.isMethod)

    /** Backing field for an accessor method, NoSymbol for all other term symbols.
     */
    def accessed: Symbol

    /** Getter method for a backing field of a val or a val, NoSymbol for all other term symbols.
     */
    def getter: Symbol

    /** Setter method for a backing field of a val or a val, NoSymbol for all other term symbols.
     */
    def setter: Symbol

    /** Does this symbol represent a field of a class
     *  that was generated from a parameter of that class?
     */
    def isParamAccessor: Boolean

    /** Does this symbol represent a field of a case class
     *  that corresponds to a parameter in the first parameter list of the
     *  primary constructor of that class?
     */
    def isCaseAccessor: Boolean

    /** Does this symbol represent a parameter with a default value?
     */
    def isParamWithDefault: Boolean

    /** Does this symbol represent a by-name parameter?
     */
    def isByNameParam: Boolean
  }

  /** The API of type symbols */
  trait TypeSymbolApi extends SymbolApi { this: TypeSymbol =>
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

    /** Is the type parameter represented by this symbol contravariant?
     */
    def isContravariant : Boolean

    /** Is the type parameter represented by this symbol contravariant?
     */
    def isCovariant     : Boolean

    /** Does this symbol represent the definition of a skolem?
     *  Skolems are used during typechecking to represent type parameters viewed from inside their scopes.
     */
    def isSkolem       : Boolean

    /** Does this symbol represent the definition of a type alias?
     */
    def isAliasType    : Boolean

    /** Does this symbol represent the definition of an abstract type?
     */
    def isAbstractType : Boolean

    /** Does this symbol represent an existentially bound type?
     */
    def isExistential  : Boolean

    /** For a polymorphic type, its type parameters, the empty list for all other types */
    def typeParams: List[Symbol]
  }

  /** The API of method symbols */
  trait MethodSymbolApi extends TermSymbolApi { this: MethodSymbol =>
    final override def isMethod = true
    final override def asMethod = this

    /** Does this method represent a constructor?
     *
     *  If `owner` is a class, then this is a vanilla JVM constructor.
     *  If `owner` is a trait, then this is a mixin constructor.
     */
    def isConstructor: Boolean

    /** Does this symbol denote the primary constructor of its enclosing class? */
    def isPrimaryConstructor: Boolean

    /** For a polymorphic method, its type parameters, the empty list for all other methods */
    def typeParams: List[Symbol]

    /** All parameter lists of the method.
     *  The name ending with "ss" indicates that the result type is a list of lists.
     *
     *  Can be used to distinguish nullary methods and methods with empty parameter lists.
     *  For a nullary method, returns the empty list (i.e. `List()`).
     *  For a method with an empty parameter list, returns a list that contains the empty list (i.e. `List(List())`).
     */
    def paramss: List[List[Symbol]]

    /** Does this method support variable length argument lists?
     */
    def isVarargs: Boolean

    /** The return type of the method */
    def returnType: Type
  }

  /** The API of module symbols */
  trait ModuleSymbolApi extends TermSymbolApi { this: ModuleSymbol =>
    /** The class implicitly associated with the object definition.
     *  One can go back from a module class to the associated module symbol
     *  by inspecting its `selfType.termSymbol`.
     */
    def moduleClass: Symbol // needed for tree traversals
    // when this becomes `moduleClass: ClassSymbol`, it will be the happiest day in my life

    final override def isModule = true
    final override def asModule = this
  }

  /** The API of class symbols */
  trait ClassSymbolApi extends TypeSymbolApi { this: ClassSymbol =>
    final override def isClass = true
    final override def asClass = this

    /** Does this symbol represent the definition of a primitive class?
     *  Namely, is it one of [[scala.Double]], [[scala.Float]], [[scala.Long]], [[scala.Int]], [[scala.Char]],
     *  [[scala.Short]], [[scala.Byte]], [[scala.Unit]] or [[scala.Boolean]]?
     */
    def isPrimitive: Boolean

    /** Does this symbol represent the definition of a numeric value class?
     *  Namely, is it one of [[scala.Double]], [[scala.Float]], [[scala.Long]], [[scala.Int]], [[scala.Char]],
     *  [[scala.Short]], [[scala.Byte]], [[scala.Unit]] or [[scala.Boolean]]?
     */
    def isNumeric: Boolean

    /** Does this symbol represent the definition of a custom value class?
     *  Namely, is AnyVal among its parent classes?
     */
    def isDerivedValueClass: Boolean

    /** Does this symbol represent a trait?
     */
    def isTrait: Boolean

    /** Does this symbol represent an abstract class?
     */
    def isAbstractClass: Boolean

    /** Does this symbol represent a case class?
     */
    def isCaseClass: Boolean

    /** Does this symbol represent a sealed class?
     */
    def isSealed: Boolean

    /** If this is a sealed class, its known direct subclasses.
     *  Otherwise, the empty set.
     */
    def knownDirectSubclasses: Set[Symbol]

    /** The list of all base classes of this type (including its own typeSymbol)
     *  in reverse linearization order, starting with the class itself and ending
     *  in class Any.
     */
    def baseClasses: List[Symbol]

    /** The module corresponding to this module class,
     *  or NoSymbol if this symbol is not a module class.
     */
    def module: Symbol

    /** If this symbol is a class or trait, its self type, otherwise the type
     *  of the symbol itself.
     */
    def selfType: Type

    /** The type `C.this`, where `C` is the current class */
    def thisPrefix: Type

    /** For a polymorphic class/trait, its type parameters, the empty list for all other classes/trait */
    def typeParams: List[Symbol]
  }

  /** The API of free term symbols */
  trait FreeTermSymbolApi extends TermSymbolApi { this: FreeTermSymbol =>
    final override def isFreeTerm = true
    final override def asFreeTerm = this

    /** The place where this symbol has been spawned */
    def origin: String

    /** The valus this symbol refers to */
    def value: Any
  }

  /** The API of free term symbols */
  trait FreeTypeSymbolApi extends TypeSymbolApi { this: FreeTypeSymbol =>
    final override def isFreeType = true
    final override def asFreeType = this

    /** The place where this symbol has been spawned */
    def origin: String
  }
}
