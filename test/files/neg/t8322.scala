
trait W[A]
class Writes[-A]

object Writes {
  def apply[A](f: A => String) = new Writes[A]()
}

trait F {

  implicit def jw[A](implicit wa: Writes[A], wj: W[Any]): W[A]

  implicit val ew: Writes[Any]

  implicit def rw[E] = Writes[Seq[E]] { _ => "" }

  implicit def wr[E] = jw(implicitly, implicitly)

  Right(0).right.flatMap(_ => new String())
}
