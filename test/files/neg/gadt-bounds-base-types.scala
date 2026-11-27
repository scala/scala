sealed trait Tag[A]
case object LS extends Tag[List[String]]
case object I  extends Tag[Int]

object Test {
  // the base types that `T` has in the first case must not be seen in the second, where `T` is `Int`
  def nextCase[T](tag: Tag[T], x: T) = tag match {
    case LS => x.map(_.length).sum
    case I  => val l = if (x > 0) x else Nil; l.size
  }

  def afterMatch[T](tag: Tag[T], x: T) = {
    tag match { case LS => x.map(_.length); case _ => }
    val l = if (tag ne null) x else Nil
    l.size
  }
}
