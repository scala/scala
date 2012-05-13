package scala.reflect
package api

trait Types { self: Universe =>

  /** This class declares operations that are visible in a Type.
   */
  abstract class AbsType {
    /** The type symbol associated with the type, or `NoSymbol` for types
     *  that do not refer to a type symbol.
     */
    def typeSymbol: Symbol

    /** The defined or declared members with name `name` in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def declaration(name: Name): Symbol

    /** The collection of declarations in this type
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
     */
    def members: Iterable[Symbol]

    /** An iterable containing all non-private members of this type (directly declared or inherited)
     *  Members appear in the linearization order of their owners.
     *  Members with the same owner appear in reverse order of their declarations.
     */
    def nonPrivateMembers: Iterable[Symbol]

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

    /** The kind of this type; used for debugging */
    def kind: String
  }

  /** An object representing an unknown type, used during type inference.
   *  If you see WildcardType outside of inference it is almost certainly a bug.
   */
  val WildcardType: Type

  /** BoundedWildcardTypes, used only during type inference, are created in
   *  two places that I can find:
   *
   *    1. If the expected type of an expression is an existential type,
   *       its hidden symbols are replaced with bounded wildcards.
   *    2. When an implicit conversion is being sought based in part on
   *       the name of a method in the converted type, a HasMethodMatching
   *       type is created: a MethodType with parameters typed as
   *       BoundedWildcardTypes.
   */
  type BoundedWildcardType >: Null <: Type

  val BoundedWildcardType: BoundedWildcardTypeExtractor

  abstract class BoundedWildcardTypeExtractor {
    def apply(bounds: TypeBounds): BoundedWildcardType
    def unapply(tpe: BoundedWildcardType): Option[TypeBounds]
  }

  /** The type of Scala types, and also Scala type signatures.
   *  (No difference is internally made between the two).
   */
  type Type >: Null <: AbsType

  /** The type of Scala singleton types, i.e. types that are inhabited
   *  by only one nun-null value. These include types of the forms
   *  {{{
   *    C.this.type
   *    C.super.type
   *    x.type
   *  }}}
   *  as well as constant types.
   */
  type SingletonType >: Null <: Type

  /** This constant is used as a special value that indicates that no meaningful type exists.
   */
  val NoType: Type

  /** This constant is used as a special value denoting the empty prefix in a path dependent type.
   *  For instance `x.type` is represented as `SingleType(NoPrefix, <x>)`, where `<x>` stands for
   *  the symbol for `x`.
   */
  val NoPrefix: Type

  /** The `ThisType` type describes types of the form on the left with the
   *  correspnding ThisType representations to the right.
   *  {{{
   *     C.this.type             ThisType(C)
   *  }}}
   */
  type ThisType <: SingletonType

  /** The constructor/deconstructor for `ThisType` instances. */
  val ThisType: ThisTypeExtractor

  /** An extractor class to create and pattern match with syntax `ThisType(sym)`
   *  where `sym` is the class prefix of the this type.
   */
  abstract class ThisTypeExtractor {
    def apply(sym: Symbol): Type
    def unapply(tpe: ThisType): Option[Symbol]
  }

  /** The `TypeRef` type describes types of any of the forms on the left,
   *  with their TypeRef representations to the right.
   *  {{{
   *     T # C[T_1, ..., T_n]      TypeRef(T, C, List(T_1, ..., T_n))
   *     p.C[T_1, ..., T_n]        TypeRef(p.type, C, List(T_1, ..., T_n))
   *     C[T_1, ..., T_n]          TypeRef(NoPrefix, C, List(T_1, ..., T_n))
   *     T # C                     TypeRef(T, C, Nil)
   *     p.C                       TypeRef(p.type, C, Nil)
   *     C                         TypeRef(NoPrefix, C, Nil)
   *  }}}
   */
  type TypeRef <: Type

  /** The constructor/deconstructor for `TypeRef` instances. */
  val TypeRef: TypeRefExtractor

  /** An extractor class to create and pattern match with syntax `TypeRef(pre, sym, args)`
   *  Here, `pre` is the prefix of the type reference, `sym` is the symbol
   *  referred to by the type reference, and `args` is a possible empty list of
   *  type argumenrts.
   */
  abstract class TypeRefExtractor {
    def apply(pre: Type, sym: Symbol, args: List[Type]): Type
    def unapply(tpe: TypeRef): Option[(Type, Symbol, List[Type])]
  }

  /** The `SingleType` type describes types of any of the forms on the left,
   *  with their TypeRef representations to the right.
   *  {{{
   *     (T # x).type             SingleType(T, x)
   *     p.x.type                 SingleType(p.type, x)
   *     x.type                   SingleType(NoPrefix, x)
   *  }}}
   */
  type SingleType <: SingletonType

  /** The constructor/deconstructor for `SingleType` instances. */
  val SingleType: SingleTypeExtractor

  /** An extractor class to create and pattern match with syntax `SingleType(pre, sym)`
   *  Here, `pre` is the prefix of the single-type, and `sym` is the stable value symbol
   *  referred to by the single-type.
   */
  abstract class SingleTypeExtractor {
    def apply(pre: Type, sym: Symbol): Type
    def unapply(tpe: SingleType): Option[(Type, Symbol)]
  }

  /** The `SuperType` type is not directly written, but arises when `C.super` is used
   *  as a prefix in a `TypeRef` or `SingleType`. It's internal presentation is
   *  {{{
   *     SuperType(thistpe, supertpe)
   *  }}}
   *  Here, `thistpe` is the type of the corresponding this-type. For instance,
   *  in the type arising from C.super, the `thistpe` part would be `ThisType(C)`.
   *  `supertpe` is the type of the super class referred to by the `super`.
   */
  type SuperType <: SingletonType

  /** The constructor/deconstructor for `SuperType` instances. */
  val SuperType: SuperTypeExtractor

  /** An extractor class to create and pattern match with syntax `SingleType(thistpe, supertpe)`
   */
  abstract class SuperTypeExtractor {
    def apply(thistpe: Type, supertpe: Type): Type
    def unapply(tpe: SuperType): Option[(Type, Type)]
  }

  /** The `ConstantType` type is not directly written in user programs, but arises as the type of a constant.
   *  The REPL expresses constant types like   Int(11).  Here are some constants with their types.
   *  {{{
   *     1           ConstantType(Constant(1))
   *     "abc"       ConstantType(Constant("abc"))
   *  }}}
   */
  type ConstantType <: SingletonType

  /** The constructor/deconstructor for `ConstantType` instances. */
  val ConstantType: ConstantTypeExtractor

  /** An extractor class to create and pattern match with syntax `ConstantType(constant)`
   *  Here, `constant` is the constant value represented by the type.
   */
  abstract class ConstantTypeExtractor {
    def apply(value: Constant): ConstantType
    def unapply(tpe: ConstantType): Option[Constant]
  }

  /** A subtype of Type representing refined types as well as `ClassInfo` signatures.
   */
  type CompoundType <: Type

  /** The `RefinedType` type defines types of any of the forms on the left,
   *  with their RefinedType representations to the right.
   *  {{{
   *     P_1 with ... with P_m { D_1; ...; D_n}      RefinedType(List(P_1, ..., P_m), Scope(D_1, ..., D_n))
   *     P_1 with ... with P_m                       RefinedType(List(P_1, ..., P_m), Scope())
   *     { D_1; ...; D_n}                            RefinedType(List(AnyRef), Scope(D_1, ..., D_n))
   *  }}}
   */
  type RefinedType <: CompoundType

  /** The constructor/deconstructor for `RefinedType` instances. */
  val RefinedType: RefinedTypeExtractor

  /** An extractor class to create and pattern match with syntax `RefinedType(parents, decls)`
   *  Here, `parents` is the list of parent types of the class, and `decls` is the scope
   *  containing all declarations in the class.
   */
  abstract class RefinedTypeExtractor {
    def apply(parents: List[Type], decls: Scope): RefinedType

    /** An alternative constructor that passes in the synthetic classs symbol
     *  that backs the refined type. (Normally, a fresh class symbol is created automatically).
     */
    def apply(parents: List[Type], decls: Scope, clazz: Symbol): RefinedType
    def unapply(tpe: RefinedType): Option[(List[Type], Scope)]
  }

  type NullaryMethodType <: Type
  val NullaryMethodType: NullaryMethodTypeExtractor

  type PolyType <: Type
  val PolyType: PolyTypeExtractor

  type ExistentialType <: Type
  val ExistentialType: ExistentialTypeExtractor

  type AnnotatedType <: Type
  val AnnotatedType: AnnotatedTypeExtractor

  /** The `MethodType` type signature is used to indicate parameters and result type of a method
   */
  type MethodType <: Type

  /** The constructor/deconstructor for `MethodType` instances. */
  val MethodType: MethodTypeExtractor

  /** An extractor class to create and pattern match with syntax `MethodType(params, respte)`
   *  Here, `params` is a potentially empty list of parameter symbols of the method,
   *  and `restpe` is the result type of the method. If the method is curried, `restpe` would
   *  be another `MethodType`.
   *  Note: `MethodType(Nil, Int)` would be the type of a method defined with an empty parameter list.
   *  {{{
   *     def f(): Int
   *  }}}
   *  If the method is completely parameterless, as in
   *  {{{
   *     def f: Int
   *  }}}
   *  its type is a `NullaryMethodType`.
   */
  abstract class MethodTypeExtractor {
    def apply(params: List[Symbol], resultType: Type): MethodType
    def unapply(tpe: MethodType): Option[(List[Symbol], Type)]
  }

  /** The `TypeBounds` type signature is used to indicate lower and upper type bounds
   *  of type parameters and abstract types. It is not a first-class type.
   *  If an abstract type or type parameter is declared with any of the forms
   *  on the left, its type signature is the TypeBounds type on the right.
   *  {{{
   *     T >: L <: U               TypeBounds(L, U)
   *     T >: L                    TypeBounds(L, Any)
   *     T <: U                    TypeBounds(Nothing, U)
   *  }}}
   */
  type TypeBounds <: Type

  /** The constructor/deconstructor for `TypeBounds` instances. */
  val TypeBounds: TypeBoundsExtractor

  /** An extractor class to create and pattern match with syntax `TypeBound(lower, upper)`
   *  Here, `lower` is the lower bound of the `TypeBounds` pair, and `upper` is
   *  the upper bound.
   */
  abstract class TypeBoundsExtractor {
    def apply(lo: Type, hi: Type): TypeBounds
    def unapply(tpe: TypeBounds): Option[(Type, Type)]
  }

  /** The `ClassInfo` type signature is used to define parents and declarations
   *  of classes, traits, and objects. If a class, trait, or object C is declared like this
   *  {{{
   *     C extends P_1 with ... with P_m { D_1; ...; D_n}
   *  }}}
   *  its `ClassInfo` type has the following form:
   *  {{{
   *     ClassInfo(List(P_1, ..., P_m), Scope(D_1, ..., D_n), C)
   *  }}}
   */
  type ClassInfoType <: CompoundType

  /** The constructor/deconstructor for `ClassInfoType` instances. */
  val ClassInfoType: ClassInfoTypeExtractor

  /** An extractor class to create and pattern match with syntax `ClassInfo(parents, decls, clazz)`
   *  Here, `parents` is the list of parent types of the class, `decls` is the scope
   *  containing all declarations in the class, and `clazz` is the symbol of the class
   *  itself.
   */
  abstract class ClassInfoTypeExtractor {
    def apply(parents: List[Type], decls: Scope, clazz: Symbol): ClassInfoType
    def unapply(tpe: ClassInfoType): Option[(List[Type], Scope, Symbol)]
  }

  abstract class NullaryMethodTypeExtractor {
    def apply(resultType: Type): NullaryMethodType
    def unapply(tpe: NullaryMethodType): Option[(Type)]
  }

  abstract class PolyTypeExtractor {
    def apply(typeParams: List[Symbol], resultType: Type): PolyType
    def unapply(tpe: PolyType): Option[(List[Symbol], Type)]
  }

  abstract class ExistentialTypeExtractor {
    def apply(quantified: List[Symbol], underlying: Type): ExistentialType
    def unapply(tpe: ExistentialType): Option[(List[Symbol], Type)]
  }

  abstract class AnnotatedTypeExtractor {
    def apply(annotations: List[AnnotationInfo], underlying: Type, selfsym: Symbol): AnnotatedType
    def unapply(tpe: AnnotatedType): Option[(List[AnnotationInfo], Type, Symbol)]
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

