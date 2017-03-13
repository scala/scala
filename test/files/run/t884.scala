import scala.util.Try

import scala.reflect.runtime.{ universe => ru }

object Test {
  def main(args: Array[String]): Unit = {
    val p = new PatternTests
    p.optImplicitTest
    p.fromStringTest
    p.associatedIntTest
    p.whatListTest
  }
}

class PatternTests {
  private def optimp[A](x: A)(implicit SA: OptImp[Show[A]], SE: OptImp[Eq[A]]): String = x match {
    case OptImp[Show](z) => "Show[Int]"
    case OptImp[Eq](z)   => "Eq[Int]"
    case _               => "Nothing"
  }

  /** Demonstrating the conversion of the scrutinee into
   *  various types and arities based on the supplied type args.
   */
  private def fromString(s: String): (String, Int) = s match {
    case FromString[Int](n)                   => "Int" -> n
    case FromString[Double](n)                => "Double" -> n.toInt
    case FromString[(Double, Double)]((a, b)) => "(Double,Double)" -> (a + b).toInt
    case _                                    => "Unknown" -> 0
  }

  /** Demonstrating calculating a value based on the
   *  supplied type argument if the scrutinee is of that type.
   */
  private def associatedInt(x: Any): String = x match {
    case AssociatedInt[Boolean](n)        => s"$x: Boolean -> $n"
    case AssociatedInt[Int](n)            => s"$x: Int -> $n"
    case AssociatedInt[Long](n)           => s"$x: Long -> $n"
    case AssociatedInt[String](n)         => s"$x: String -> $n"
    case AssociatedInt[Array[Int]](n)     => s"Array[Int] -> $n"
    case AssociatedInt[Array[Long]](n)    => s"Array[Long] -> $n"
    case AssociatedInt[Array[Boolean]](n) => s"Array[Boolean] -> $n"
    case AssociatedInt[Array[Float]](n)   => s"Array[Float] -> $n"
    case _                                => s"$x: ?"
  }
  private def whatList[A: ru.TypeTag](xs: List[A]): String = xs match {
    case IsType[List[String]]() => "string"
    case IsType[List[Int]]()    => "int"
    case _                      => "blub"
  }

  def optImplicitTest(): Unit = {
    println("optImplicitTest");

    {
      implicit val z = new Show[Int] { }
      println(optimp(0))
    }

    {
      implicit val z = new Eq[Int] { }
      println(optimp(0))
    }

    println(optimp(0))
  }

  def fromStringTest(): Unit = {
    println("fromStringTest")
    println(fromString("55"))
    println(fromString("99.9"))
    println(fromString("12.3,45.6"))
    println(fromString("donkey"))
  }

  def associatedIntTest(): Unit = {
    println("associatedIntTest")
    println(associatedInt("abcdefghijklmnopqrstuvwxyz"))
    println(associatedInt(true))
    println(associatedInt(false))
    println(associatedInt(Array[Boolean](true, true, true, false)))
    println(associatedInt(71))
    println(associatedInt(981L))
    println(associatedInt(Array[Int](1, 2, 3, 4)))
    println(associatedInt(Array[Long](1, 2, 3, 4, 5)))
  }
  def whatListTest(): Unit = {
    println("whatListTest")
    println(whatList(List[String]()))
    println(whatList(List[Int]()))
    println(whatList(List[Boolean]()))
  }
}

/*** Auxiliary classes. ***/

trait Show[A]
trait Eq[A]

sealed trait OptImp[+A] {
  def toOption: Option[A] = this match {
    case Empty() => scala.None
    case Some(x) => scala.Some(x)
  }
}
final case class Empty[A]() extends OptImp[A]
final case class Some[A](x: A) extends OptImp[A]

object OptImp {
  implicit def convert[A](implicit z: A = null.asInstanceOf[A]): OptImp[A] =
    if (z == null) Empty() else Some(z)

  class Aux[F[_]] {
    def apply[A](x: A)(implicit z: OptImp[F[A]]): Option[F[A]] = z.toOption
  }

  def unapply[F[_]]: Aux[F] = new Aux[F]
}

object IsType {
  import scala.reflect.runtime.universe._

  class Aux[A] {
    def apply[V](x: V)(implicit vt: TypeTag[V], at: TypeTag[A]): Boolean = typeOf[V] <:< typeOf[A]
  }

  def unapply[A]: Aux[A] = new Aux[A]
}

trait FromString[A] {
  def apply(s: String): Option[A]
}
object FromString {
  def unapply[R] : Helper[R] = new Helper

  def apply[A](f: String => Option[A]): FromString[A]                       = new FromString[A] { def apply(x: String) = f(x) }
  def partial[A](pf: PartialFunction[String, A]): FromString[A]             = apply[A](pf.lift)
  def partialFlat[A](pf: PartialFunction[String, Option[A]]): FromString[A] = apply[A](pf lift _ flatten)

  implicit val intFromString: FromString[Int]       = apply(s => Try(s.toInt).toOption)
  implicit val doubleFromString: FromString[Double] = apply(s => Try(s.toDouble).toOption)

  implicit val doubleDoubleFromString: FromString[(Double, Double)] = {
    val Regex = """^([^,]+),(.*)$""".r
    FromString partialFlat {
      case Regex(a, b) => Try((a.toDouble, b.toDouble)).toOption
      case _           => None
    }
  }

  class Helper[R] {
    def apply(s: String)(implicit z: FromString[R]): Option[R] = z(s)
  }
}

trait AssociatedInt[A] {
  def associated(x: A): Int
}
object AssociatedInt {
  def unapply[A] = new Helper[A]

  class Helper[A] {
    def apply(x: Any)(implicit z: AssociatedInt[A] = null): Option[Int] =
      if (z eq null) None else Try(z associated x.asInstanceOf[A]).toOption
  }

  implicit val booleanAssoc: AssociatedInt[Boolean]                                 = make(b => if (b) 1 else 0)
  implicit val intAssoc: AssociatedInt[Int]                                         = make(_ => 4)
  implicit val longAssoc: AssociatedInt[Long]                                       = make(_ => 8)
  implicit val stringAssoc: AssociatedInt[String]                                   = make(_.length)
  implicit def arrayAssoc[A](implicit z: AssociatedInt[A]): AssociatedInt[Array[A]] = make(_ map z.associated sum)

  private def make[A](f: A => Int): AssociatedInt[A] = new AssociatedInt[A] { def associated(x: A): Int = f(x) }
}
