package example

class Weehawken
object Weehawken {
  var flippity = 1
  type Blippitx = Int
}

class A {
  Weehawken.flippitx = 123
  Nil map identiyt
  new example.Eeehawken
}

object B {
  import scala.io.StdIn.{readline, readInt}
}

object C {
  import scala.io.stdin.{readLine => line}
}

class Hohokus {
  protected def bar1: Unit = ()
  protected[example] def bar2: Unit = ()
}
object Hohokus {
  def foo1 = 1
  def foo2 = 2
  def foo3 = 3
  def foo4 = 4
  def foo5 = 5
  def foo6 = 6
}

object D {
  Hohokus.foo
}

object E {
  new Hohokus().bar // don't suggest bar1
}

object assignments {
  class C {
    def abc(i: Int) = i
    def +++(i: Int) = i
    var x = 42
    val xx = 42
    var z = 42
    var zz = 42
  }
  val c = new C
  def f = c.acb(42)
  def g = c.++-(42)
  def h = c.++=(42)
  def i = c ++= 42
  def u = c x_= 1
  def v = c y_= 1
  def v2 = c.y = 1
  def w = c x_== 1
  def y = c.xx_=(1)
  def y2 = c.xx = 1
  def z = c.zz_=(1)
  def z2 = c.zzz_=(1)
  def z3 = c.++++(1)
  def legacy_duple = c.xxx  // did not suggest c.xx as too short
}

class MaxAlt {
  def miss0 = 42
  def miss1 = 42
  def miss2 = 42
  def miss3 = 42
  def miss33 = 42
  def test: Int = new MaxAlt().missN
}

class MissAlt {
  def miss0 = 42
  def miss1 = 42
  def miss2 = 42
  def test: Int = new MissAlt().missN
}

class MoreAlt {
  def miss0 = 42
  def miss1 = 42
  def miss2 = 42
  def miss3 = 42
  def miss4 = 42
  def miss5 = 42
  def miss6 = 42
  def test: Int = new MoreAlt().missN
}
