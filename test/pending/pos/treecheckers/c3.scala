import scala.collection.mutable.ArrayOps

object Test3 {
  implicit def genericArrayOps[T](xs: Array[T]): ArrayOps[T] = (xs match {
    case x: Array[AnyRef]  => refArrayOps[AnyRef](x)
    case x: Array[Boolean] => booleanArrayOps(x)
  }).asInstanceOf[ArrayOps[T]]
}
