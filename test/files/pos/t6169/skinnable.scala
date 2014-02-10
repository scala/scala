object ObjectProperty {
  implicit def jfxObjectProperty2sfx[T](p: OP[T]) = new ObjectProperty[T](p)
}

class ObjectProperty[T](val delegate: OP[T])

trait TestWildcardBoundInference {
  def delegate: Skinnable
  def skin: ObjectProperty[Skin[_ /* inferred: <: Skinnable */]] = ObjectProperty.jfxObjectProperty2sfx(delegate.skinProperty)
  skin: ObjectProperty[Skin[_  <: Skinnable]]

  def skinCheckInference = delegate.skinProperty
  skinCheckInference: ObjectProperty[Skin[_  <: Skinnable]]
}