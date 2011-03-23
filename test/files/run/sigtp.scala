import scala.tools.partest._

trait BugBase [A, E] {
  val key: A
  var next: E = _
}

final class Bug[A, B](val key: A) extends BugBase[A, Bug[A, B]] {
  def foo = next
}

object Test extends App with SigTest {
  show[BugBase[_, _]]()
  show[Bug[_, _]]()
}
