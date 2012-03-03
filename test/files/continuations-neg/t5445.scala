import scala.util.continuations._

object Test {
  def foo(block: Unit @suspendable ): Unit @suspendable = {}
}
