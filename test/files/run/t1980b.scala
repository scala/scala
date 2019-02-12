import language.implicitConversions
import collection.immutable.LazyList
import scala.util.chaining._

case class LI(n: Option[Int], rest: LI) {
  def ##:: (elem: Cell): LI = LI(Some(elem.x), this)
  def  #:: (s: String):  LI = LI(Some(s.toInt), this)

  case class Cell(x: Int)
  object Cell {
    implicit def `wrap element`(x: Int): Cell = Cell(x).tap(println)
  }
}

object LI {
  def empty: LI = LI(None, null)
}

class C {
  def i = { println("I"); 42 }
  def j = { println("J"); 27 }
  def f = j :: i :: Nil
  def g = j #:: i #:: LazyList.empty
  def k = i ##:: LI.empty
  def r = j ::[Int] i ::[Int] Nil
  def s = "x"*2 ::[Unit] Nil // discarded
}

class D {
  def f(i: Int) = i.tap(n => println(s"f$n"))
  def g(i: Int) = i.toString.tap(n => println(s"g$n"))
  def xs = LI.empty.tap(_ => println("empty"))
  def test = f(27) ##:: g(5) #:: f(42) ##:: xs
}

// check order of evaluation with mixed stabilizers
object Test extends App {
  val c = new C
  println(c.f)
  println(c.g)
  println(c.k)
  println(c.r)
  println(c.s)
  val d = new D
  println(d.test)
}
