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
  case class Dongoo
  @serializable case class Heyooooo 

  @deprecated("I'm an ironic deprecation warning") def f0 = 5 // where's this disappearing?
  def f1 = Double.Epsilon   // and this?
}

:warnings
  """
}
