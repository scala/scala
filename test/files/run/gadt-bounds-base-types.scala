import scala.reflect.{ClassTag, classTag}

trait Tag[A]
case object LS extends Tag[List[String]]
case object I  extends Tag[Int]
case object J  extends Tag[Integer]
case object O  extends Tag[Option[Int]]

trait Box[A]
class LBox extends Box[List[String]]
class IBox extends Box[Int]

// A case narrows the bounds of `T` to what its pattern implies. Base types computed from the narrowed bounds
// must not be seen once the case is left: the lubs below are taken where `T` is unbounded again (or bounded
// differently), so they must not be `List[String]`.
object Test {
  def show[A: ClassTag](a: A): Unit = println(s"${classTag[A].runtimeClass.getName} $a")

  def control[T](tag: Tag[T], x: T) = {
    val l = if (tag ne null) x else Nil
    l
  }

  def afterMatch[T](tag: Tag[T], x: T) = {
    tag match { case LS => x.map(_.length); case _ => }
    val l = if (tag ne null) x else Nil
    l
  }

  def nextCase[T](tag: Tag[T], x: T) = tag match {
    case LS => x.map(_.length).sum
    case I  => val l = if (x > 0) x else Nil; l
  }

  def head[T](tag: Tag[T], x: T) = {
    tag match { case LS => x.head; case _ => }
    val l = if (tag ne null) x else Nil
    l
  }

  def foreach[T](tag: Tag[T], x: T) = {
    tag match { case LS => x.foreach(_ => ()); case _ => }
    val l = if (tag ne null) x else Nil
    l
  }

  def filter[T](tag: Tag[T], x: T) = {
    tag match { case LS => x.filter(_ => true); case _ => }
    val l = if (tag ne null) x else Nil
    l
  }

  def typedPattern[T](b: Box[T], x: T) = {
    b match { case _: LBox => x.map(_.length); case _ => }
    val l = if (b ne null) x else Nil
    l
  }

  // types that take their base types from `T`
  def bounded[T, U <: T](tag: Tag[T], x: U) = {
    tag match { case LS => x.map(_.length); case _ => }
    val l = if (tag ne null) x else Nil
    l
  }

  def compound[T](tag: Tag[T], x: T with Serializable) = {
    tag match { case LS => x.map(_.length); case _ => }
    val l = if (tag ne null) x else Nil
    l
  }

  def alias[T](tag: Tag[T], x0: T) = {
    type A = T
    val x: A = x0
    tag match { case LS => x.map(_.length); case _ => }
    val l = if (tag ne null) x else Nil
    l
  }

  def applied[F[_], T](tag: Tag[F[T]], x: F[T]) = {
    tag match { case LS => x.head; case _ => }
    val l = if (tag ne null) x else Nil
    l
  }

  // the bounds of the outer case are in force again after the inner match
  def nested[T](tag: Tag[T], tag2: Tag[T], x: T): Int = tag match {
    case LS =>
      tag2 match { case LS => x.head; case _ => }
      val l = if (tag ne null) x else Nil
      l.size
    case _ => -1
  }

  def main(args: Array[String]): Unit = {
    show(control(I, 1))
    show(afterMatch(I, 1))
    show(nextCase(I, 1))
    show(head(I, 1))
    show(foreach(I, 1))
    show(filter(I, 1))
    show(typedPattern(new IBox, 1))
    show(bounded[Int, Int](I, 1))
    show(compound[Integer](J, 1))
    show(alias(I, 1))
    show(applied[Option, Int](O, Some(1)))
    show(nested(LS, LS, List("a", "b")))
    show(afterMatch(LS, List("a")))
  }
}
