import scala.collection.ArrayOps
object Test {
  def genericArrayOps[T](xs: Array[T]): ArrayOps[T] = xs match {
    case x: Array[AnyRef]  => refArrayOps[AnyRef](x).asInstanceOf[ArrayOps[T]]
    case null              => null.asInstanceOf[ArrayOps[T]] // `ArrayOps` is AnyVal
  }
  // def genericArrayOps[T >: Nothing <: Any](xs: Array[T]): scala.collection.mutable.ArrayOps[T]
  //   = OptionMatching.runOrElse(xs)(((x1: Array[T]) =>
  //     ((OptionMatching.guard(x1.isInstanceOf[Array[AnyRef]], x1.asInstanceOf[Array[T] with Array[AnyRef]]).flatMap(((x2: Array[T] with Array[AnyRef]) =>
  //       OptionMatching.one(Test.this.refArrayOps[AnyRef](x2).asInstanceOf[scala.collection.mutable.ArrayOps[T]]))): Option[scala.collection.mutable.ArrayOps[T]]).orElse(
  //     (OptionMatching.guard(null.==(x1), x1.asInstanceOf[Array[T]]).flatMap(((x3: Array[T]) =>
  //         OptionMatching.one(null))): Option[scala.collection.mutable.ArrayOps[T]])): Option[scala.collection.mutable.ArrayOps[T]]).orElse((OptionMatching.zero: Option[scala.collection.mutable.ArrayOps[T]]))))

  def refArrayOps[T <: AnyRef](xs: Array[T]): ArrayOps[T] = collection.arrayToArrayOps(xs)
}