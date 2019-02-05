
import scala.language.postfixOps

import scala.collection.{ mutable, immutable, generic }

object Test {
  type PerturberFn[T] = IterableOnce[T] => IterableOnce[T]
  lazy val Id = new Perturber(Nil, identity[IterableOnce[Int]] _) { }
  class Perturber(val labels: List[String], val f: PerturberFn[Int]) extends PerturberFn[Int] {
    def apply(xs: IterableOnce[Int]): IterableOnce[Int] = f(xs)
    def show(xs: IterableOnce[Int]): String = {
      val res       = f(xs)
      val resString = "" + res
      val rest      = LazyList.from(res)
      val failed    = (rest take 100).size == 100

      "%-45s %-30s %s".format(toString, resString,
        if (failed) "<failed>" else rest.mkString(" ")
      )
    }
    def and(g: Perturber): Perturber =
      new Perturber(this.labels ++ g.labels, f andThen g.f)

    override def toString = labels mkString " -> "
  }
  object Perturber {
    def apply(label: String, f: PerturberFn[Int]) = new Perturber(List(label), f)
  }

  def naturals = LazyList from 1
  val toV : Perturber = Perturber("view", LazyList.from(_).view)
  val toI : Perturber = Perturber("iterator", _.iterator)
  val toS : Perturber = Perturber("LazyList.from", LazyList.from(_))
  val toIS : Perturber = Perturber("IndexedSeq.from", IndexedSeq.from(_))

  def p(ps: Perturber*): Perturber = if (ps.isEmpty) Id else ps.reduceLeft(_ and _)
  def drop(n: Int): Perturber = Perturber("drop " + n, _.iterator drop n)
  def take(n: Int): Perturber = Perturber("take " + n, _.iterator take n)
  def slice(from: Int, until: Int): Perturber =
    Perturber(
      "slice(%d, %d)".format(from, until),
      LazyList.from(_).slice(from, until)
    )

  val fns = List[Perturber](toV, toI, toS, toIS)

  def tds(n: Int): Perturber = p(drop(n), take(n / 2), slice(1, n / 4))
  def dts(n: Int): Perturber = p(take(n), drop(n / 2), slice(1, n / 4))
  def sdt(n: Int): Perturber = p(slice(n, n * 2), drop(n / 2), take(n / 4))
  def std(n: Int): Perturber = p(slice(n, n * 2), take(n / 2), drop(n / 4))

  val transforms = (fns.permutations map (xs => p(xs take 3: _*))).toList.distinct
  def mkOps(n: Int) = List[Perturber](tds(n), dts(n), sdt(n), std(n))
  def runOps(n: Int) = {
    val xs: List[(String, List[String])] = mkOps(n) map { op =>
      ("" + op, transforms map (_ show op(naturals)) sorted)
    }
    for ((k, v) <- xs) {
      println("\n** " + k + " **\n")
      println("-------------------")
      v foreach println
    }
    ()
  }

  def main(args: Array[String]): Unit = {
    runOps(20)
  }
}
