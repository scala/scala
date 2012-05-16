import scala.sth

object Test extends App {
  def foo(a: Int)(implicit b: sth.Sth): Unit = {}
  foo(1)

  def bar(x: Int)(implicit y: Int): sth.Sth = null
  bar(1)

  def meh(x: Int)(implicit a: sth.Sth, b: Int): Unit = {}
  meh(1)

  def meh2(x: Int)(implicit b: Int, a: sth.Sth): Unit = {}
  meh2(1)
}

