import language.implicitConversions
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

// check order of evaluation with mixed stabilizers
object Test extends App {
  def f(i: Int) = i.tap(n => println(s"f$n"))
  def g(i: Int) = i.toString.tap(n => println(s"g$n"))
  def xs = LI.empty.tap(_ => println("empty"))
  f(27) ##:: g(5) #:: f(42) ##:: xs
}
