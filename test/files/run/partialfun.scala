import collection._
import collection.generic._

object Test {
  def collectIDA[A, B, Repr, That](_this: TraversableLike[A, Repr])(pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val repr: Repr = _this.asInstanceOf[Repr]
    val b = bf(repr)
    _this foreach { x => if (pf isDefinedAt x) b += pf(x) }
    b.result
  }

  def collectRW[A, B, Repr, That](_this: TraversableLike[A, Repr])(pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val repr: Repr = _this.asInstanceOf[Repr]
    val b = bf(repr)
    val f = pf runWith { b += _ }
    _this foreach f
    b.result
  }

  var cnt = 0

  object Ex1 {
    def unapply(x: Int) : Option[Int] = {
      cnt += 1
      if ((x % 3) == 0) Some(-x) else None
    }
  }

  object Ex2 {
    def unapply(x: Int) : Option[Int] = {
      //cnt += 1
      if ((x % 5) == 0) Some(x) else None
    }
  }

  def resetCnt() = { val r = cnt; cnt = 0; r }

  val pf: PartialFunction[Int,Int] = {
    case Ex1(result) => result
    case Ex2(result) => result
  }

  def collectTest() {
    val xs = 1 to 100
    resetCnt()

    val ysIDA = collectIDA(xs)(pf)
    val cntIDA = resetCnt()

    val ysRW = collectRW(xs)(pf)
    val cntRW = resetCnt()

    val ys = xs collect pf

    assert(ys == ysIDA)
    assert(ys == ysRW)
    assert(cntIDA == xs.length + ys.length)
    assert(cntRW == xs.length)
    println(ys.length)
    println(cntIDA)
    println(cntRW)
  }

  def orElseTest() {
    val pf0 = new PartialFunction[Unit, Unit] {
      def apply(u: Unit) { println("0:apply") }
      def isDefinedAt(u: Unit) = { println("0:isDefinedAt"); false }
    }
    val pf1 = new PartialFunction[Unit, Unit] {
      def apply(u: Unit) { println("1:apply") }
      def isDefinedAt(u: Unit) = { println("1:isDefinedAt"); false }
    }
    val pf2 = new PartialFunction[Unit, Unit] {
      def apply(u: Unit) { println("2:apply") }
      def isDefinedAt(u: Unit) = { println("2:isDefinedAt"); true }
    }

    val chained = pf0 orElse pf1 orElse pf2
    chained(())
  }

  def main(args: Array[String]): Unit = {
    collectTest()
    orElseTest()
  }
}
