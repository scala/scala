package scala.reflect
package api

/**
 * Defines the type hierachy for types.
 *
 * Note: Because of implementation details, some type factories have return type `Type`
 * instead of a more precise type.
 *
 * @see [[scala.reflect]] for a description on how the class hierarchy is encoded here.
 */
trait Types { self: Universe =>

  /** The type of Scala types, and also Scala type signatures.
   *  (No difference is internally made between the two).
   */
  type Type >: Null <: TypeApi

  /** A tag that preserves the identity of the `Type` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val TypeTagg: ClassTag[Type]

  /** This constant is used as a special value that indicates that no meaningful type exists.
   */
  val NoType: Type

  /** This constant is used as a special value denoting the empty prefix in a path dependent type.
   *  For instance `x.type` is represented as `SingleType(NoPrefix, <x>)`, where `<x>` stands for
   *  the symbol for `x`.
   */
  val NoPrefix: Type

  /** The API of types
   */
  abstract class TypeApi {
    /** The term symbol associated with the type, or `NoSymbol` for types
     *  that do not refer to a term symbol.
     */
    def termSymbol: Symbol

    /** The type symbol associated with the type, or `NoSymbol` for types
     *  that do not refer to a type symbol.
     */
    def typeSymbol: Symbol

    /** The defined or declared members with name `name` in this type;
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     *  Alternatives of overloaded symbol appear in the order they are declared.
     */
    def declaration(name: Name): Symbol

    /** A `Scope` containing directly declared members of this type.
     *  Unlike `members` this method doesn't returns inherited members.
     *
     *  Members in the returned scope might appear in arbitrary order.
     *  Use `declarations.sorted` to get an ordered list of members.
     */
    def declarations: MemberScope

    /** The member with given name, either directly declared or inherited,
     *  an OverloadedSymbol if several exist, NoSymbol if none exist.
     */
    def member(name: Name): Symbol

    /** A `Scope` containing all members of this type (directly declared or inherited).
     *  Unlike `declarations` this method also returns inherited members.
     *
     *  Members in the returned scope might appear in arbitrary order.
     *  Use `declarations.sorted` to get an ordered list of members.
     */
    def members: MemberScope

    /** Is this type a type constructor that is missing its type arguments?
     */
    def takesTypeArgs: Boolean

    /** Returns the corresponding type constructor (e.g. List for List[T] or List[String])
     */
    def typeConstructor: Type

    /**
     *  Expands type aliases and converts higher-kinded TypeRefs to PolyTypes.
     *  Functions on types are also implemented as PolyTypes.
     *
     *  Example: (in the below, <List> is the type constructor of List)
     *    TypeRef(pre, <List>, List()) is replaced by
     *    PolyType(X, TypeRef(pre, <List>, List(X)))
     */
    def normalize: Type

    /** Does this type conform to given type argument `that`? */
    def <:< (that: Type): Boolean

    /** Is this type a weak subtype of that type? True also for numeric types, i.e. Int weak_<:< Long.
     */
    def weak_<:<(that: Type): Boolean

    /** Is this type equivalent to given type argument `that`? */
    def =:= (that: Type): Boolean

    /** The list of all base classes of this type (including its own typeSymbol)
     *  in reverse linearization order, starting with the class itself and ending
     *  in class Any.
     */
    def baseClasses: List[Symbol]

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
    def erasure: Type

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

    /******************* helpers *******************/

    /** Substitute symbols in `to` for corresponding occurrences of references to
     *  symbols `from` in this type.
     */
    def substituteSymbols(from: List[Symbol], to: List[Symbol]): Type

    /** Substitute types in `to` for corresponding occurrences of references to
     *  symbols `from` in this type.
     */
    def substituteTypes(from: List[Symbol], to: List[Type]): Type

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
  }

  /** The type of Scala singleton types, i.e., types that are inhabited
   *  by only one nun-null value. These include types of the forms
   *  {{{
   *    C.this.type
   *    C.super.type
   *    x.type
   *  }}}
   *  as well as [[ConstantType constant types]].
   */
  type SingletonType >: Null <: Type

  /** A tag that preserves the identity of the `SingletonType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val SingletonTypeTag: ClassTag[SingletonType]

  /** A singleton type that describes types of the form on the left with the
   *  corresponding `ThisType` representation to the right:
   *  {{{
   *     C.this.type             ThisType(C)
   *  }}}
   */
  type ThisType >: Null <: AnyRef with SingletonType with ThisTypeApi

  /** A tag that preserves the identity of the `ThisType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ThisTypeTag: ClassTag[ThisType]

  /** The constructor/deconstructor for `ThisType` instances. */
  val ThisType: ThisTypeExtractor

  /** An extractor class to create and pattern match with syntax `ThisType(sym)`
   *  where `sym` is the class prefix of the this type.
   */
  abstract class ThisTypeExtractor {
    /**
     * Creates a ThisType from the given class symbol.
     */
    def apply(sym: Symbol): Type
    def unapply(tpe: ThisType): Option[Symbol]
  }

  /** The API that all this types support */
  trait ThisTypeApi extends TypeApi { this: ThisType =>
    val sym: Symbol
  }

  /** The `SingleType` type describes types of any of the forms on the left,
   *  with their TypeRef representations to the right.
   *  {{{
   *     (T # x).type             SingleType(T, x)
   *     p.x.type                 SingleType(p.type, x)
   *     x.type                   SingleType(NoPrefix, x)
   *  }}}
   */
  type SingleType >: Null <: AnyRef with SingletonType with SingleTypeApi

  /** A tag that preserves the identity of the `SingleType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val SingleTypeTag: ClassTag[SingleType]

  /** The constructor/deconstructor for `SingleType` instances. */
  val SingleType: SingleTypeExtractor

  /** An extractor class to create and pattern match with syntax `SingleType(pre, sym)`
   *  Here, `pre` is the prefix of the single-type, and `sym` is the stable value symbol
   *  referred to by the single-type.
   */
  abstract class SingleTypeExtractor {
    def apply(pre: Type, sym: Symbol): Type // not SingleTypebecause of implementation details
    def unapply(tpe: SingleType): Option[(Type, Symbol)]
  }

  /** The API that all single types support */
  trait SingleTypeApi extends TypeApi { this: SingleType =>
    val pre: Type
    val sym: Symbol
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
  type SuperType >: Null <: AnyRef with SingletonType with SuperTypeApi

  /** A tag that preserves the identity of the `SuperType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val SuperTypeTag: ClassTag[SuperType]

  /** The constructor/deconstructor for `SuperType` instances. */
  val SuperType: SuperTypeExtractor

  /** An extractor class to create and pattern match with syntax `SingleType(thistpe, supertpe)`
   */
  abstract class SuperTypeExtractor {
    def apply(thistpe: Type, supertpe: Type): Type // not SuperTypebecause of implementation details
    def unapply(tpe: SuperType): Option[(Type, Type)]
  }

  /** The API that all super types support */
  trait SuperTypeApi extends TypeApi { this: SuperType =>
    val thistpe: Type
    val supertpe: Type
  }
  /** The `ConstantType` type is not directly written in user programs, but arises as the type of a constant.
   *  The REPL expresses constant types like `Int(11)`. Here are some constants with their types:
   *  {{{
   *     1           ConstantType(Constant(1))
   *     "abc"       ConstantType(Constant("abc"))
   *  }}}
   */
  type ConstantType >: Null <: AnyRef with SingletonType with ConstantTypeApi

  /** A tag that preserves the identity of the `ConstantType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ConstantTypeTag: ClassTag[ConstantType]

  /** The constructor/deconstructor for `ConstantType` instances. */
  val ConstantType: ConstantTypeExtractor

  /** An extractor class to create and pattern match with syntax `ConstantType(constant)`
   *  Here, `constant` is the constant value represented by the type.
   */
  abstract class ConstantTypeExtractor {
    def apply(value: Constant): ConstantType
    def unapply(tpe: ConstantType): Option[Constant]
  }

  /** The API that all constant types support */
  trait ConstantTypeApi extends TypeApi { this: ConstantType =>
    val value: Constant
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
  type TypeRef >: Null <: AnyRef with Type with TypeRefApi

  /** A tag that preserves the identity of the `TypeRef` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val TypeRefTag: ClassTag[TypeRef]

  /** The constructor/deconstructor for `TypeRef` instances. */
  val TypeRef: TypeRefExtractor

  /** An extractor class to create and pattern match with syntax `TypeRef(pre, sym, args)`
   *  Here, `pre` is the prefix of the type reference, `sym` is the symbol
   *  referred to by the type reference, and `args` is a possible empty list of
   *  type argumenrts.
   */
  abstract class TypeRefExtractor {
    def apply(pre: Type, sym: Symbol, args: List[Type]): Type // not TypeRefbecause of implementation details
    def unapply(tpe: TypeRef): Option[(Type, Symbol, List[Type])]
  }

  /** The API that all type refs support */
  trait TypeRefApi extends TypeApi { this: TypeRef =>
    val pre: Type
    val sym: Symbol
    val args: List[Type]
  }

  /** A subtype of Type representing refined types as well as `ClassInfo` signatures.
   */
  type CompoundType >: Null <: AnyRef with Type

  /** A tag that preserves the identity of the `CompoundType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val CompoundTypeTag: ClassTag[CompoundType]

  /** The `RefinedType` type defines types of any of the forms on the left,
   *  with their RefinedType representations to the right.
   *  {{{
   *     P_1 with ... with P_m { D_1; ...; D_n}      RefinedType(List(P_1, ..., P_m), Scope(D_1, ..., D_n))
   *     P_1 with ... with P_m                       RefinedType(List(P_1, ..., P_m), Scope())
   *     { D_1; ...; D_n}                            RefinedType(List(AnyRef), Scope(D_1, ..., D_n))
   *  }}}
   */
  type RefinedType >: Null <: AnyRef with CompoundType with RefinedTypeApi

  /** A tag that preserves the identity of the `RefinedType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val RefinedTypeTag: ClassTag[RefinedType]

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

  /** The API that all refined types support */
  trait RefinedTypeApi extends TypeApi { this: RefinedType =>
    val parents: List[Type]
    val decls: Scope
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
  type ClassInfoType >: Null <: AnyRef with CompoundType with ClassInfoTypeApi

  /** A tag that preserves the identity of the `ClassInfoType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ClassInfoTypeTag: ClassTag[ClassInfoType]

  /** The constructor/deconstructor for `ClassInfoType` instances. */
  val ClassInfoType: ClassInfoTypeExtractor

  /** An extractor class to create and pattern match with syntax `ClassInfo(parents, decls, clazz)`
   *  Here, `parents` is the list of parent types of the class, `decls` is the scope
   *  containing all declarations in the class, and `clazz` is the symbol of the class
   *  itself.
   */
  abstract class ClassInfoTypeExtractor {
    def apply(parents: List[Type], decls: Scope, typeSymbol: Symbol): ClassInfoType
    def unapply(tpe: ClassInfoType): Option[(List[Type], Scope, Symbol)]
  }

  /** The API that all class info types support */
  trait ClassInfoTypeApi extends TypeApi { this: ClassInfoType =>
    val parents: List[Type]
    val decls: Scope
    val typeSymbol: Symbol
  }

  /** The `MethodType` type signature is used to indicate parameters and result type of a method
   */
  type MethodType >: Null <: AnyRef with Type with MethodTypeApi

  /** A tag that preserves the identity of the `MethodType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val MethodTypeTag: ClassTag[MethodType]

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

  /** The API that all method types support */
  trait MethodTypeApi extends TypeApi { this: MethodType =>
    val params: List[Symbol]
    val resultType: Type
  }

  /** The `NullaryMethodType` type signature is used for parameterless methods
   *  with declarations of the form `def foo: T`
   */
  type NullaryMethodType >: Null <: AnyRef with Type with NullaryMethodTypeApi

  /** A tag that preserves the identity of the `NullaryMethodType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val NullaryMethodTypeTag: ClassTag[NullaryMethodType]

  /** The constructor/deconstructor for `NullaryMethodType` instances. */
  val NullaryMethodType: NullaryMethodTypeExtractor

  /** An extractor class to create and pattern match with syntax `NullaryMethodType(resultType)`.
   *  Here, `resultType` is the result type of the parameterless method.
   */
  abstract class NullaryMethodTypeExtractor {
    def apply(resultType: Type): NullaryMethodType
    def unapply(tpe: NullaryMethodType): Option[(Type)]
  }

  /** The API that all nullary method types support */
  trait NullaryMethodTypeApi extends TypeApi { this: NullaryMethodType =>
    val resultType: Type
  }

  /** The `PolyType` type signature is used for polymorphic methods
   *  that have at least one type parameter.
   */
  type PolyType >: Null <: AnyRef with Type with PolyTypeApi

  /** A tag that preserves the identity of the `PolyType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val PolyTypeTag: ClassTag[PolyType]

  /** The constructor/deconstructor for `PolyType` instances. */
  val PolyType: PolyTypeExtractor

  /** An extractor class to create and pattern match with syntax `PolyType(typeParams, resultType)`.
   *  Here, `typeParams` are the type parameters of the method and `resultType`
   *  is the type signature following the type parameters.
   */
  abstract class PolyTypeExtractor {
    def apply(typeParams: List[Symbol], resultType: Type): PolyType
    def unapply(tpe: PolyType): Option[(List[Symbol], Type)]
  }

  /** The API that all polymorphic types support */
  trait PolyTypeApi extends TypeApi { this: PolyType =>
    val typeParams: List[Symbol]
    val resultType: Type
  }

  /** The `ExistentialType` type signature is used for existential types and
   *  wildcard types.
   */
  type ExistentialType >: Null <: AnyRef with Type with ExistentialTypeApi

  /** A tag that preserves the identity of the `ExistentialType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ExistentialTypeTag: ClassTag[ExistentialType]

  /** The constructor/deconstructor for `ExistentialType` instances. */
  val ExistentialType: ExistentialTypeExtractor

  /** An extractor class to create and pattern match with syntax
   *  `ExistentialType(quantified, underlying)`.
   *  Here, `quantified` are the type variables bound by the existential type and `underlying`
   *  is the type that's existentially quantified.
   */
  abstract class ExistentialTypeExtractor {
    def apply(quantified: List[Symbol], underlying: Type): ExistentialType
    def unapply(tpe: ExistentialType): Option[(List[Symbol], Type)]
  }

  /** The API that all existential types support */
  trait ExistentialTypeApi extends TypeApi { this: ExistentialType =>
    val quantified: List[Symbol]
    val underlying: Type
  }

  /** The `AnnotatedType` type signature is used for annotated types of the
   *  for `<type> @<annotation>`.
   */
  type AnnotatedType >: Null <: AnyRef with Type with AnnotatedTypeApi

  /** A tag that preserves the identity of the `AnnotatedType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val AnnotatedTypeTag: ClassTag[AnnotatedType]

  /** The constructor/deconstructor for `AnnotatedType` instances. */
  val AnnotatedType: AnnotatedTypeExtractor

  /** An extractor class to create and pattern match with syntax
   * `AnnotatedType(annotations, underlying, selfsym)`.
   *  Here, `annotations` are the annotations decorating the underlying type `underlying`.
   *  `selfSym` is a symbol representing the annotated type itself.
   */
  abstract class AnnotatedTypeExtractor {
    def apply(annotations: List[Annotation], underlying: Type, selfsym: Symbol): AnnotatedType
    def unapply(tpe: AnnotatedType): Option[(List[Annotation], Type, Symbol)]
  }

  /** The API that all annotated types support */
  trait AnnotatedTypeApi extends TypeApi { this: AnnotatedType =>
    val annotations: List[Annotation]
    val underlying: Type
    val selfsym: Symbol
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
  type TypeBounds >: Null <: AnyRef with Type with TypeBoundsApi

  /** A tag that preserves the identity of the `TypeBounds` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val TypeBoundsTag: ClassTag[TypeBounds]

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

  /** The API that all type bounds support */
  trait TypeBoundsApi extends TypeApi { this: TypeBounds =>
    val lo: Type
    val hi: Type
  }

  /** An object representing an unknown type, used during type inference.
   *  If you see WildcardType outside of inference it is almost certainly a bug.
   */
  val WildcardType: Type

  /** BoundedWildcardTypes, used only during type inference, are created in
   *  two places:
   *
   *    1. If the expected type of an expression is an existential type,
   *       its hidden symbols are replaced with bounded wildcards.
   *    2. When an implicit conversion is being sought based in part on
   *       the name of a method in the converted type, a HasMethodMatching
   *       type is created: a MethodType with parameters typed as
   *       BoundedWildcardTypes.
   */
  type BoundedWildcardType >: Null <: AnyRef with Type with BoundedWildcardTypeApi

  /** A tag that preserves the identity of the `BoundedWildcardType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val BoundedWildcardTypeTag: ClassTag[BoundedWildcardType]

  /** The constructor/deconstructor for `BoundedWildcardType` instances. */
  val BoundedWildcardType: BoundedWildcardTypeExtractor

  /** An extractor class to create and pattern match with syntax `BoundedWildcardTypeExtractor(bounds)`
   *  with `bounds` denoting the type bounds.
   */
  abstract class BoundedWildcardTypeExtractor {
    def apply(bounds: TypeBounds): BoundedWildcardType
    def unapply(tpe: BoundedWildcardType): Option[TypeBounds]
  }

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
