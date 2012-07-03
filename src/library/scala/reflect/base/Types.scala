package scala.reflect
package base

trait Types { self: Universe =>

  /** The base API that all types support */
  abstract class TypeBase {

    /** The term symbol associated with the type, or `NoSymbol` for types
     *  that do not refer to a term symbol.
     */
    def termSymbol: Symbol

    /** The type symbol associated with the type, or `NoSymbol` for types
     *  that do not refer to a type symbol.
     */
    def typeSymbol: Symbol
  }

  /** The type of Scala types, and also Scala type signatures.
   *  (No difference is internally made between the two).
   */
  type Type >: Null <: TypeBase

  /** A tag that preserves the identity of the `Type` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val TypeTagg: ClassTag[Type] // [Eugene++] rename!

  /** This constant is used as a special value that indicates that no meaningful type exists.
   */
  val NoType: Type

  /** This constant is used as a special value denoting the empty prefix in a path dependent type.
   *  For instance `x.type` is represented as `SingleType(NoPrefix, <x>)`, where `<x>` stands for
   *  the symbol for `x`.
   */
  val NoPrefix: Type

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

  /** A tag that preserves the identity of the `SingletonType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val SingletonTypeTag: ClassTag[SingletonType]

  /** The `ThisType` type describes types of the form on the left with the
   *  correspnding ThisType representations to the right.
   *  {{{
   *     C.this.type             ThisType(C)
   *  }}}
   */
  type ThisType >: Null <: AnyRef with SingletonType

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
    def apply(sym: Symbol): Type // not ThisTypebecause of implementation details
    def unapply(tpe: ThisType): Option[Symbol]
  }

  /** The `SingleType` type describes types of any of the forms on the left,
   *  with their TypeRef representations to the right.
   *  {{{
   *     (T # x).type             SingleType(T, x)
   *     p.x.type                 SingleType(p.type, x)
   *     x.type                   SingleType(NoPrefix, x)
   *  }}}
   */
  type SingleType >: Null <: AnyRef with SingletonType

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

  /** The `SuperType` type is not directly written, but arises when `C.super` is used
   *  as a prefix in a `TypeRef` or `SingleType`. It's internal presentation is
   *  {{{
   *     SuperType(thistpe, supertpe)
   *  }}}
   *  Here, `thistpe` is the type of the corresponding this-type. For instance,
   *  in the type arising from C.super, the `thistpe` part would be `ThisType(C)`.
   *  `supertpe` is the type of the super class referred to by the `super`.
   */
  type SuperType >: Null <: AnyRef with SingletonType

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

  /** The `ConstantType` type is not directly written in user programs, but arises as the type of a constant.
   *  The REPL expresses constant types like   Int(11).  Here are some constants with their types.
   *  {{{
   *     1           ConstantType(Constant(1))
   *     "abc"       ConstantType(Constant("abc"))
   *  }}}
   */
  type ConstantType >: Null <: AnyRef with SingletonType

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
  type TypeRef >: Null <: AnyRef with Type

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
  type RefinedType >: Null <: AnyRef with CompoundType

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
  type ClassInfoType >: Null <: AnyRef with CompoundType

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

  /** The `MethodType` type signature is used to indicate parameters and result type of a method
   */
  type MethodType >: Null <: AnyRef with Type

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

  /** The `NullaryMethodType` type signature is used for parameterless methods
   *  with declarations of the form `def foo: T`
   */
  type NullaryMethodType >: Null <: AnyRef with Type

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

  /** The `PolyType` type signature is used for polymorphic methods
   *  that have at least one type parameter.
   */
  type PolyType >: Null <: AnyRef with Type

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

  /** The `ExistentialType` type signature is used for existential types and
   *  wildcard types.
   */
  type ExistentialType >: Null <: AnyRef with Type

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

  /** The `AnnotatedType` type signature is used for annotated types of the
   *  for `<type> @<annotation>`.
   */
  type AnnotatedType >: Null <: AnyRef with Type

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
    def apply(annotations: List[AnnotationInfo], underlying: Type, selfsym: Symbol): AnnotatedType
    def unapply(tpe: AnnotatedType): Option[(List[AnnotationInfo], Type, Symbol)]
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
  type TypeBounds >: Null <: AnyRef with Type

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
  type BoundedWildcardType >: Null <: AnyRef with Type

  /** A tag that preserves the identity of the `BoundedWildcardType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val BoundedWildcardTypeTag: ClassTag[BoundedWildcardType]

  val BoundedWildcardType: BoundedWildcardTypeExtractor

  abstract class BoundedWildcardTypeExtractor {
    def apply(bounds: TypeBounds): BoundedWildcardType
    def unapply(tpe: BoundedWildcardType): Option[TypeBounds]
  }
}
