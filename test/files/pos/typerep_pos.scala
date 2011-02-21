object typerep extends App {
  class TypeRep[T] {}
  case object IntRep extends TypeRep[Int] {
    override def toString = "Int"
  }
  case object BooleanRep extends TypeRep[Boolean] {
    override def toString = "Boolean"
  }
  case class ListRep[T](elemrep: TypeRep[T]) extends TypeRep[List[T]] {
    override def toString = "List"
  }

  implicit def intRep: TypeRep[Int] = IntRep
  implicit def booleanRep: TypeRep[Boolean] = BooleanRep
  implicit def listRep[T](implicit elemrep: TypeRep[T]): TypeRep[List[T]] = ListRep(elemrep)

  def getType[T](x: T)(implicit rep: TypeRep[T]): TypeRep[T] = rep

  println(getType(1))
  println(getType(List(1)))
}
