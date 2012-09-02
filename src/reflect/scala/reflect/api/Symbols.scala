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

  /** The API of symbols */
  trait SymbolApi extends SymbolBase { this: Symbol =>

    /** Source file if this symbol is created during this compilation run,
     *  or a class file if this symbol is loaded from a *.class or *.jar.
     */
    def associatedFile: scala.tools.nsc.io.AbstractFile

    /** A list of annotations attached to this Symbol.
     */
    // we cannot expose the `annotations` method because it doesn't auto-initialize a symbol (see SI-5423)
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
  trait TermSymbolApi extends SymbolApi with TermSymbolBase { this: TermSymbol =>
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

    /** Does this symbol represent an implicit value, definition or parameter?
     */
    def isImplicit: Boolean

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
  trait TypeSymbolApi extends SymbolApi with TypeSymbolBase { this: TypeSymbol =>
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
  trait MethodSymbolApi extends TermSymbolApi with MethodSymbolBase { this: MethodSymbol =>
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
     *
     *  Can be used to distinguish nullary methods and methods with empty parameter lists.
     *  For a nullary method, returns the empty list (i.e. `List()`).
     *  For a method with an empty parameter list, returns a list that contains the empty list (i.e. `List(List())`).
     */
    def params: List[List[Symbol]]

    /** Does this method support variable length argument lists?
     */
    def isVarargs: Boolean

    /** The return type of the method */
    def returnType: Type
  }

  /** The API of module symbols */
  trait ModuleSymbolApi extends TermSymbolApi with ModuleSymbolBase { this: ModuleSymbol =>
  }

  /** The API of class symbols */
  trait ClassSymbolApi extends TypeSymbolApi with ClassSymbolBase { this: ClassSymbol =>
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
