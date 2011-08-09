import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """

// "Is this thing on?" Not working on first couple
// commands, needs investigation.
123
123
123

object o {
  case class Bippy()
  case class Dingus {
    def f[T](xs: TraversableOnce[T]) = xs match {
      case _: List[Int]   => 1
      case _: Set[String] => 2
      case _              => xs.isInstanceOf[Iterator[Double]]
    }
  }
  case class DingDangDoobie(ding: Int, dang: Int, doobie: Double)
  case class Dongoo ; case class Heyooooo ; for (x <- 1 to 10 ; val y = x ; z = y) yield x
}
:warnings
  """
}
