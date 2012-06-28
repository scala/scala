package scala.reflect
package api

trait Symbols extends base.Symbols { self: Universe =>

  override type Symbol >: Null <: SymbolApi
  override type TypeSymbol >: Null <: Symbol with TypeSymbolApi
  override type TermSymbol >: Null <: Symbol with TermSymbolApi
  override type MethodSymbol >: Null <: TermSymbol with MethodSymbolApi
  override type ModuleSymbol >: Null <: TermSymbol with ModuleSymbolApi
  override type ClassSymbol >: Null <: TypeSymbol with ClassSymbolApi
  override type FreeTermSymbol >: Null <: TermSymbol with FreeTermSymbolApi
  override type FreeTypeSymbol >: Null <: TypeSymbol with FreeTypeSymbolApi

  trait HasFlagsApi {
    def flags: FlagSet
    def hasFlag(fs: FlagSet): Boolean
    def hasAllFlags(fs: FlagSet): Boolean
    def flagString: String
  }

  /** The API of symbols */
  trait SymbolApi extends SymbolBase with HasFlagsApi { this: Symbol =>

    /** The position of this symbol
     */
    def pos: Position

    /** A list of annotations attached to this Symbol.
     */
    // [Eugene++] we cannot expose the `annotations` method because it doesn't auto-initialize a symbol (see SI-5423)
    // there was an idea to use the `isCompilerUniverse` flag and auto-initialize symbols in `annotations` whenever this flag is false
    // but it doesn't work, because the unpickler (that is shared between reflective universes and global universes) is very picky about initialization
    // scala.reflect.internal.Types$TypeError: bad reference while unpickling scala.collection.immutable.Nil: type Nothing not found in scala.type not found.
    //        at scala.reflect.internal.pickling.UnPickler$Scan.toTypeError(UnPickler.scala:836)
    //        at scala.reflect.internal.pickling.UnPickler$Scan$LazyTypeRef.complete(UnPickler.scala:849)          // auto-initialize goes boom
    //        at scala.reflect.internal.Symbols$Symbol.info(Symbols.scala:1140)
    //        at scala.reflect.internal.Symbols$Symbol.initialize(Symbols.scala:1272)                              // this triggers auto-initialize
    //        at scala.reflect.internal.Symbols$Symbol.annotations(Symbols.scala:1438)                             // unpickler first tries to get pre-existing annotations
    //        at scala.reflect.internal.Symbols$Symbol.addAnnotation(Symbols.scala:1458)                           // unpickler tries to add the annotation being read
    //        at scala.reflect.internal.pickling.UnPickler$Scan.readSymbolAnnotation(UnPickler.scala:489)          // unpickler detects an annotation
    //        at scala.reflect.internal.pickling.UnPickler$Scan.run(UnPickler.scala:88)
    //        at scala.reflect.internal.pickling.UnPickler.unpickle(UnPickler.scala:37)
    //        at scala.reflect.runtime.JavaMirrors$JavaMirror.unpickleClass(JavaMirrors.scala:253)                 // unpickle from within a reflexive mirror
    //    def annotations: List[AnnotationInfo]
    def getAnnotations: List[AnnotationInfo]

    /** Whether this symbol carries an annotation for which the given
     *  symbol is its typeSymbol.
     */
    def hasAnnotation(sym: Symbol): Boolean

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
     *  java private:     hasFlag(PRIVATE)                && (privateWithin == NoSymbol)
     *  java package:     !hasFlag(PRIVATE | PROTECTED)   && (privateWithin == enclosingPackage)
     *  java protected:   hasFlag(PROTECTED)              && (privateWithin == enclosingPackage)
     *  java public:      !hasFlag(PRIVATE | PROTECTED)   && (privateWithin == NoSymbol)
     */
    def privateWithin: Symbol

    /** For a class: the module or case class factory with the same name in the same package.
     *  For a module: the class with the same name in the same package.
     *  For all others: NoSymbol
     */
    def companionSymbol: Symbol

    /** If this symbol is a package class, this symbol; otherwise the next enclosing
     *  package class, or `NoSymbol` if none exists.
     */
    def enclosingPackageClass: Symbol

    /** If this symbol is a top-level class, this symbol; otherwise the next enclosing
     *  top-level class, or `NoSymbol` if none exists.
     */
    def enclosingTopLevelClass: Symbol

    /** Does this symbol represent a value, i.e. not a module and not a method?
     *  If yes, `isTerm` is also guaranteed to be true.
     *  [Eugene++] I need a review of the implementation
     */
    def isValue: Boolean

    /** Does this symbol denote a stable value? */
    def isStable: Boolean

    /** Does this symbol represent a mutable value?
     *  If yes, `isTerm` and `isValue` are also guaranteed to be true.
     */
    def isVariable: Boolean

    /** Does this symbol represent a getter or a setter?
     */
    def isAccessor: Boolean

    /** Does this symbol represent a getter of a field?
     *  If yes, `isTerm` and `isMethod` are also guaranteed to be true.
     */
    def isGetter: Boolean

    /** Does this symbol represent a setter of a field?
     *  If yes, `isTerm` and `isMethod` are also guaranteed to be true.
     */
    def isSetter: Boolean

    /** Does this symbol represent the definition of a package?
     *  If yes, `isTerm` is also guaranteed to be true.
     */
    def isPackage: Boolean

    /** Does this symbol represent a package class?
     *  If yes, `isClass` is also guaranteed to be true.
     */
    def isPackageClass: Boolean

    /** Is this symbol an overloaded method?
     */
    def isOverloaded   : Boolean

    /** Does this symbol represent the definition of a primitive class?
     *  Namely, is it one of [[scala.Double]], [[scala.Float]], [[scala.Long]], [[scala.Int]], [[scala.Char]],
     *  [[scala.Short]], [[scala.Byte]], [[scala.Unit]] or [[scala.Boolean]]?
     */
    def isPrimitiveValueClass: Boolean

    /** Does this symbol represent the definition of a numeric value class?
     *  Namely, is it one of [[scala.Double]], [[scala.Float]], [[scala.Long]], [[scala.Int]], [[scala.Char]],
     *  [[scala.Short]], [[scala.Byte]], [[scala.Unit]] or [[scala.Boolean]]?
     */
    def isNumericValueClass: Boolean

    /** Does this symbol represent the definition of a custom value class?
     *  Namely, is AnyVal among its parent classes?
     *  TODO: Why not just have in reflect.internal?
     *  [Eugene++] because it's useful for macros
     */
    def isDerivedValueClass: Boolean

    /** Does this symbol represent the definition of a type alias?
     *  If yes, `isType` is also guaranteed to be true.
     */
    def isAliasType    : Boolean

    /** Does this symbol represent the definition of an abstract type?
     *  If yes, `isType` is also guaranteed to be true.
     */
    def isAbstractType : Boolean

    /** Does this symbol represent an existentially bound type?
     *  If yes, `isType` is also guaranteed to be true.
     */
    def isExistential  : Boolean

    /** Does this symbol represent a free type captured by reification?
     */
    def isFreeType     : Boolean

   /** Does this symbol or its underlying type represent a typechecking error?
     */
    def isErroneous : Boolean

    /** Can this symbol be loaded by a reflective mirror?
     *
     *  Scalac relies on `ScalaSignature' annotation to retain symbols across compilation runs.
     *  Such annotations (also called "pickles") are applied on top-level classes and include information
     *  about all symbols reachable from the annotee. However, local symbols (e.g. classes or definitions local to a block)
     *  are typically unreachable and information about them gets lost.
     *
     *  This method is useful for macro writers who wish to save certain ASTs to be used at runtime.
     *  With `isLocatable' it's possible to check whether a tree can be retained as is, or it needs special treatment.
     */
    def isLocatable: Boolean

    /** Is this symbol static (i.e. with no outer instance)?
     *  Q: When exactly is a sym marked as STATIC?
     *  A: If it's a member of a toplevel object, or of an object contained in a toplevel object, or any number of levels deep.
     *  http://groups.google.com/group/scala-internals/browse_thread/thread/d385bcd60b08faf6
     */
    def isStatic: Boolean

    /** The type signature of this symbol seen as a member of given type `site`.
     */
    def typeSignatureIn(site: Type): Type

    /** The type signature of this symbol.
     *  Note if the symbol is a member of a class, one almost always is interested
     *  in `typeSignatureIn` with a site type instead.
     */
    def typeSignature: Type

    /** The string discriminator of this symbol; useful for debugging */
    def kind: String
  }

  /** The API of term symbols */
  trait TermSymbolApi extends SymbolApi with HasFlagsApi with TermSymbolBase { this: TermSymbol =>
    /** The overloaded alternatives of this symbol */
    def alternatives: List[Symbol]

    def resolveOverloaded(pre: Type = NoPrefix, targs: Seq[Type] = List(), actuals: Seq[Type]): Symbol
  }

  /** The API of type symbols */
  trait TypeSymbolApi extends SymbolApi with HasFlagsApi with TypeSymbolBase { this: TypeSymbol =>
    /** Is the type parameter represented by this symbol contravariant?
     */
    def isContravariant : Boolean

    /** Is the type parameter represented by this symbol contravariant?
     */
    def isCovariant     : Boolean

    /** Does this symbol represent the definition of a skolem?
     *  Skolems are used during typechecking to represent type parameters viewed from inside their scopes.
     *  If yes, `isType` is also guaranteed to be true.
     */
    def isSkolem       : Boolean

    /** A type reference that refers to this type symbol seen
     *  as a member of given type `site`.
     */
    def asTypeIn(site: Type): Type

    /**  A type reference that refers to this type symbol
      *  Note if symbol is a member of a class, one almost always is interested
      *  in `asTypeIn` with a site type instead.
      *
      *  Example: Given a class declaration `class C[T] { ... } `, that generates a symbol
      *  `C`. Then `C.asType` is the type `C[T]`.
      *
      *  By contrast, `C.typeSignature` would be a type signature of form
      *  `PolyType(ClassInfoType(...))` that describes type parameters, value
      *  parameters, parent types, and members of `C`.
      */
     def asType: Type  // !!! Same as typeSignature.
  }

  /** The API of method symbols */
  type MethodSymbolApi = MethodSymbolBase

  /** The API of module symbols */
  type ModuleSymbolApi = ModuleSymbolBase

  /** The API of class symbols */
  trait ClassSymbolApi extends TypeSymbolApi with ClassSymbolBase { this: ClassSymbol =>
    /** If this symbol is a class or trait, its self type, otherwise the type
     *  of the symbol itself.
     */
    def selfType: Type

    /** The type `C.this`, where `C` is the current class */
    def thisPrefix: Type
  }

  /** The API of free term symbols */
  trait FreeTermSymbolApi extends TermSymbolApi with FreeTermSymbolBase { this: FreeTermSymbol =>
    /** The place where this symbol has been spawned */
    def origin: String

    /** The valus this symbol refers to */
    def value: Any
  }

  /** The API of free term symbols */
  trait FreeTypeSymbolApi extends TypeSymbolApi with FreeTypeSymbolBase { this: FreeTypeSymbol =>
    /** The place where this symbol has been spawned */
    def origin: String
  }
}
