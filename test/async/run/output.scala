//> using options -Xasync

import scala.tools.partest.async.{OutputAwait, Output}
import scala.collection.immutable
import OutputAwait._

object Test {
  def v1 = Output("v1", ("line" -> "1"))
  def v2 = Output("v2", ("line" -> "2"), ("foo", "bar"))
  def test: Output[String] = writing {
    value(v1) + value(v2)
  }
  def main(args: Array[String]): Unit = {
    val result = test
    assert(result == Output(Some("v1v2"), immutable.HashMap("line" -> Vector("1", "2"), "foo" -> Vector("bar"))), result)
  }
}
