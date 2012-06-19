package scala.reflect
package api

trait Types extends base.Types { self: Universe =>

  override type Type >: Null <: TypeApi

  /** The extended API of types
   */
  abstract class TypeApi extends TypeBase {

    /** The defined or declared members with name `name` in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def declaration(name: Name): Symbol

    /** The collection of declarations in this type
     *  [Eugene++] why not List?
     */
    def declarations: Iterable[Symbol]

    /** The member with given name, either directly declared or inherited,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     */
    def member(name: Name): Symbol

    /** The non-private member with given name, either directly declared or inherited,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     */
    def nonPrivateMember(name: Name): Symbol

    /** An iterable containing all members of this type (directly declared or inherited)
     *  Members appear in the linearization order of their owners.
     *  Members with the same owner appear in reverse order of their declarations.
     *  [Eugene++] the order needs to be reversed back, at least in the public API
     */
    def members: Iterable[Symbol]

    /** An iterable containing all non-private members of this type (directly declared or inherited)
     *  Members appear in the linearization order of their owners.
     *  Members with the same owner appear in reverse order of their declarations.
     */
    def nonPrivateMembers: Iterable[Symbol]

    /** Substitute symbols in `to` for corresponding occurrences of references to
     *  symbols `from` in this type.
     */
    def substituteSymbols(from: List[Symbol], to: List[Symbol]): Type

    /** Substitute types in `to` for corresponding occurrences of references to
     *  symbols `from` in this type.
     */
    def substituteTypes(from: List[Symbol], to: List[Type]): Type

    /** If this is a parameterized types, the type arguments.
     *  Otherwise the empty list
     */
    def typeArguments: List[Type]

    /** For a (potentially wrapped) poly type, its type parameters,
     *  the empty list for all other types */
    def typeParams: List[Symbol]

    /** Is this type a type constructor that is missing its type arguments?
     */
    def isHigherKinded: Boolean   // !!! This should be called "isTypeConstructor", no?

    /** Returns the corresponding type constructor (e.g. List for List[T] or List[String])
     */
    def typeConstructor: Type

    /** Does this type refer to spliceable types or is a spliceable type?
     */
    def isConcrete: Boolean

    /** Is this type an abstract type that needs to be resolved?
     */
    def isSpliceable: Boolean

    /**
     *  Expands type aliases and converts higher-kinded TypeRefs to PolyTypes.
     *  Functions on types are also implemented as PolyTypes.
     *
     *  Example: (in the below, <List> is the type constructor of List)
     *    TypeRef(pre, <List>, List()) is replaced by
     *    PolyType(X, TypeRef(pre, <List>, List(X)))
     */
    def normalize: Type     // !!! Alternative name? "normalize" is used to mean too many things.

    /** Does this type conform to given type argument `that`? */
    def <:< (that: Type): Boolean

    /** Is this type equivalent to given type argument `that`? */
    def =:= (that: Type): Boolean

    /** The list of all base classes of this type (including its own typeSymbol)
     *  in reverse linearization order, starting with the class itself and ending
     *  in class Any.
     */
    def baseClasses: List[Symbol]   // !!! Alternative name, perhaps linearization?

    /** The least type instance of given class which is a supertype
     *  of this type.  Example:
     *  {{{
     *    class D[T]
     *    class C extends p.D[Int]
     *    ThisType(C).baseType(D) = p.D[Int]
     * }}}
     */
    def baseType(clazz: Symbol): Type

    /** This type as seen from prefix `pre` and class `clazz`. This means:
     *  Replace all thistypes of `clazz` or one of its subclasses
     *  by `pre` and instantiate all parameters by arguments of `pre`.
     *  Proceed analogously for thistypes referring to outer classes.
     *
     *  Example:
     *  {{{
     *    class D[T] { def m: T }
     *    class C extends p.D[Int]
     *    T.asSeenFrom(ThisType(C), D)  (where D is owner of m)
     *      = Int
     *  }}}
     */
    def asSeenFrom(pre: Type, clazz: Symbol): Type

    /** The erased type corresponding to this type after
     *  all transformations from Scala to Java have been performed.
     */
    def erasure: Type    // !!! "erasedType", compare with "widen" (so "erase") or "underlying" (so "erased")
                         // why not name it "erasure"?

   /** Apply `f` to each part of this type, returning
    *  a new type. children get mapped before their parents */
    def map(f: Type => Type): Type

    /** Apply `f` to each part of this type, for side effects only */
    def foreach(f: Type => Unit)

    /** Returns optionally first type (in a preorder traversal) which satisfies predicate `p`,
     *  or None if none exists.
     */
    def find(p: Type => Boolean): Option[Type]

    /** Is there part of this type which satisfies predicate `p`? */
    def exists(p: Type => Boolean): Boolean

    /** Does this type contain a reference to given symbol? */
    def contains(sym: Symbol): Boolean

    /** If this is a compound type, the list of its parent types;
     *  otherwise the empty list
     */
    def parents: List[Type]

    /** If this is a singleton type, returns the type underlying it;
     *  otherwise returns this type itself.
     */
    def underlying: Type

    /** If this is a singleton type, widen it to its nearest underlying non-singleton
     *  base type by applying one or more `underlying` dereferences.
     *  If this is not a singleton type, returns this type itself.
     *
     *  Example:
     *
     *  class Outer { class C ; val x: C }
     *  val o: Outer
     *  <o.x.type>.widen = o.C
     */
    def widen: Type

    /** Map to a singleton type which is a subtype of this type.
     *  The fallback implemented here gives:
     *  {{{
     *    T.narrow  =  (T {}).this.type
     *  }}}
     *  Overridden where we know more about where types come from.
     */
    def narrow: Type

    /** The string discriminator of this type; useful for debugging */
    def kind: String
  }

  /** .. */
  override type ThisType >: Null <: SingletonType with ThisTypeApi

  /** The API that all this types support */
  trait ThisTypeApi extends TypeApi { this: ThisType =>
    val sym: Symbol
  }

  /** .. */
  override type SingleType >: Null <: SingletonType with SingleTypeApi

  /** The API that all single types support */
  trait SingleTypeApi extends TypeApi { this: SingleType =>
    val pre: Type
    val sym: Symbol
  }

  /** .. */
  override type SuperType >: Null <: SingletonType with SuperTypeApi

  /** The API that all super types support */
  trait SuperTypeApi extends TypeApi { this: SuperType =>
    val thistpe: Type
    val supertpe: Type
  }

  /** .. */
  override type ConstantType >: Null <: SingletonType with ConstantTypeApi

  /** The API that all constant types support */
  trait ConstantTypeApi extends TypeApi { this: ConstantType =>
    val value: Constant
  }

  /** .. */
  override type TypeRef >: Null <: Type with TypeRefApi

  /** The API that all type refs support */
  trait TypeRefApi extends TypeApi { this: TypeRef =>
    val pre: Type
    val sym: Symbol
    val args: List[Type]
  }

  /** .. */
  override type RefinedType >: Null <: CompoundType with RefinedTypeApi

  /** The API that all refined types support */
  trait RefinedTypeApi extends TypeApi { this: RefinedType =>
    val parents: List[Type]
    val decls: Scope
  }

  /** .. */
  override type ClassInfoType >: Null <: CompoundType with ClassInfoTypeApi

  /** The API that all class info types support */
  trait ClassInfoTypeApi extends TypeApi { this: ClassInfoType =>
    val parents: List[Type]
    val decls: Scope
    val typeSymbol: Symbol
  }

  /** .. */
  override type MethodType >: Null <: Type with MethodTypeApi

  /** The API that all method types support */
  trait MethodTypeApi extends TypeApi { this: MethodType =>
    val params: List[Symbol]
    val resultType: Type
  }

  /** .. */
  override type NullaryMethodType >: Null <: Type with NullaryMethodTypeApi

  /** The API that all nullary method types support */
  trait NullaryMethodTypeApi extends TypeApi { this: NullaryMethodType =>
    val resultType: Type
  }

  /** .. */
  override type PolyType >: Null <: Type with PolyTypeApi

  /** The API that all polymorphic types support */
  trait PolyTypeApi extends TypeApi { this: PolyType =>
    val typeParams: List[Symbol]
    val resultType: Type
  }

  /** .. */
  override type ExistentialType >: Null <: Type with ExistentialTypeApi

  /** The API that all existential types support */
  trait ExistentialTypeApi extends TypeApi { this: ExistentialType =>
    val quantified: List[Symbol]
    val underlying: Type
  }

  /** .. */
  override type AnnotatedType >: Null <: Type with AnnotatedTypeApi

  /** The API that all annotated types support */
  trait AnnotatedTypeApi extends TypeApi { this: AnnotatedType =>
    val annotations: List[AnnotationInfo]
    val underlying: Type
    val selfsym: Symbol
  }

  /** .. */
  override type TypeBounds >: Null <: Type with TypeBoundsApi

  /** The API that all type bounds support */
  trait TypeBoundsApi extends TypeApi { this: TypeBounds =>
    val lo: Type
    val hi: Type
  }

  /** .. */
  override type BoundedWildcardType >: Null <: Type with BoundedWildcardTypeApi

  /** The API that all this types support */
  trait BoundedWildcardTypeApi extends TypeApi { this: BoundedWildcardType =>
    val bounds: TypeBounds
  }

  /** The least upper bound of a list of types, as determined by `<:<`.  */
  def lub(xs: List[Type]): Type

    /** The greatest lower bound of a list of types, as determined by `<:<`. */
  def glb(ts: List[Type]): Type

  // Creators ---------------------------------------------------------------
  // too useful and too non-trivial to be left out of public API
  // [Eugene to Paul] needs review!

  /** The canonical creator for single-types */
  def singleType(pre: Type, sym: Symbol): Type

  /** the canonical creator for a refined type with a given scope */
  def refinedType(parents: List[Type], owner: Symbol, decls: Scope, pos: Position): Type

  /** The canonical creator for a refined type with an initially empty scope.
   */
  def refinedType(parents: List[Type], owner: Symbol): Type

  /** The canonical creator for typerefs
   */
  def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type

  /** A creator for intersection type where intersections of a single type are
   *  replaced by the type itself. */
  def intersectionType(tps: List[Type]): Type

  /** A creator for intersection type where intersections of a single type are
   *  replaced by the type itself, and repeated parent classes are merged.
   *
   *  !!! Repeated parent classes are not merged - is this a bug in the
   *  comment or in the code?
   */
  def intersectionType(tps: List[Type], owner: Symbol): Type

  /** A creator for type applications */
  def appliedType(tycon: Type, args: List[Type]): Type

  /** A creator for type parameterizations that strips empty type parameter lists.
   *  Use this factory method to indicate the type has kind * (it's a polymorphic value)
   *  until we start tracking explicit kinds equivalent to typeFun (except that the latter requires tparams nonEmpty).
   */
  def polyType(tparams: List[Symbol], tpe: Type): Type

  /** A creator for existential types. This generates:
   *
   *  {{{
   *    tpe1 where { tparams }
   *  }}}
   *
   *  where `tpe1` is the result of extrapolating `tpe` with regard to `tparams`.
   *  Extrapolating means that type variables in `tparams` occurring
   *  in covariant positions are replaced by upper bounds, (minus any
   *  SingletonClass markers), type variables in `tparams` occurring in
   *  contravariant positions are replaced by upper bounds, provided the
   *  resulting type is legal with regard to stability, and does not contain
   *  any type variable in `tparams`.
   *
   *  The abstraction drops all type parameters that are not directly or
   *  indirectly referenced by type `tpe1`. If there are no remaining type
   *  parameters, simply returns result type `tpe`.
   */
  def existentialAbstraction(tparams: List[Symbol], tpe0: Type): Type
}
