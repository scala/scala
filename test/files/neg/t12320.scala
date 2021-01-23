// scalac: -Werror -Xlint
//
trait T {
  def f = Option.empty[Int].contains("") || false
  def g(other: Option[Int]) = other.contains("") || false
  def any = Option.empty[Int].contains("")
}
