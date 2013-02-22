package scala.reflect
package api

trait ImplicitTags {
  self: Types =>

  /** A tag that preserves the identity of the `Type` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val TypeTagg: ClassTag[Type]

  /** A tag that preserves the identity of the `SingletonType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val SingletonTypeTag: ClassTag[SingletonType]

  /** A tag that preserves the identity of the `ThisType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val ThisTypeTag: ClassTag[ThisType]

  /** A tag that preserves the identity of the `SingleType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val SingleTypeTag: ClassTag[SingleType]

  /** A tag that preserves the identity of the `SuperType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val SuperTypeTag: ClassTag[SuperType]

  /** A tag that preserves the identity of the `ConstantType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val ConstantTypeTag: ClassTag[ConstantType]

  /** A tag that preserves the identity of the `TypeRef` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val TypeRefTag: ClassTag[TypeRef]

  /** A tag that preserves the identity of the `CompoundType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val CompoundTypeTag: ClassTag[CompoundType]

  /** A tag that preserves the identity of the `RefinedType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val RefinedTypeTag: ClassTag[RefinedType]

  /** A tag that preserves the identity of the `ClassInfoType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val ClassInfoTypeTag: ClassTag[ClassInfoType]

  /** A tag that preserves the identity of the `MethodType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val MethodTypeTag: ClassTag[MethodType]

  /** A tag that preserves the identity of the `NullaryMethodType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val NullaryMethodTypeTag: ClassTag[NullaryMethodType]

  /** A tag that preserves the identity of the `PolyType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val PolyTypeTag: ClassTag[PolyType]

  /** A tag that preserves the identity of the `ExistentialType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val ExistentialTypeTag: ClassTag[ExistentialType]

  /** A tag that preserves the identity of the `AnnotatedType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val AnnotatedTypeTag: ClassTag[AnnotatedType]

  /** A tag that preserves the identity of the `TypeBounds` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val TypeBoundsTag: ClassTag[TypeBounds]

  /** A tag that preserves the identity of the `BoundedWildcardType` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val BoundedWildcardTypeTag: ClassTag[BoundedWildcardType]
}
