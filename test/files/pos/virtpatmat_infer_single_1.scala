case class TypeBounds(a: Type, b: Type)
class Type {
  def bounds: TypeBounds = bounds match {
    case TypeBounds(_: this.type, _: this.type) => TypeBounds(this, this)
    case oftp                                   => oftp
  }
}