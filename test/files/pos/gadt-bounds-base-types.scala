sealed trait Tag[A]
case object LS extends Tag[List[String]]

object Test {
  // base types of `T`, or of a type derived from it, computed before the match must not hide the bounds
  // that the case gives `T`
  def f[T](tag: Tag[T], x: T): Int = {
    val l0 = if (tag ne null) x else Nil
    tag match { case LS => x.map(_.length).sum; case _ => 0 }
  }

  def g[T](tag: Tag[T], x: T): Int = {
    val l0 = if (tag ne null) x else Nil
    tag match {
      case LS => val l = if (tag ne null) x else Nil; l.size
      case _  => 0
    }
  }

  def bounded[T, U <: T](tag: Tag[T], x: U): Int = {
    val l0 = if (tag ne null) x else Nil
    tag match { case LS => x.map(_.length).sum; case _ => 0 }
  }

  def alias[T](tag: Tag[T], x0: T): Int = {
    type A = T
    val x: A = x0
    val l0 = if (tag ne null) x else Nil
    tag match { case LS => x.map(_.length).sum; case _ => 0 }
  }
}
