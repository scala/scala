package tastytest

final class Prod private (private val total: Int) extends AnyVal {

  def * (other: Prod): Prod = new Prod(total * other.total)
  def mul (other: Prod): Prod = new Prod(total * other.total)

}

object Prod {

  val empty: Prod = new Prod(0)

  def apply(x: Int): Option[Prod] = if (x < 1) None else Some(new Prod(x))

}
