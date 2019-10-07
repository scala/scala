import collection._
import collection.generic._

object Test {
  def collectIDA[A, B, CC[_], Repr, That](_this: IterableOps[A, CC, Repr])(pf: PartialFunction[A, B])(implicit bf: BuildFrom[Repr, B, That]): That = {
    val repr: Repr = _this.asInstanceOf[Repr]
    val b = bf.newBuilder(repr)
    _this foreach { x => if (pf isDefinedAt x) b += pf(x) }
    b.result
  }

  def collectRW[A, B, CC[_], Repr, That](_this: IterableOps[A, CC, Repr])(pf: PartialFunction[A, B])(implicit bf: BuildFrom[Repr, B, That]): That = {
    val repr: Repr = _this.asInstanceOf[Repr]
    val b = bf.newBuilder(repr)
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

  def collectTest(): Unit = {
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

  def orElseTest(): Unit = {
    val pf0 = new PartialFunction[Unit, Unit] {
      def apply(u: Unit): Unit = { println("0:apply") }
      def isDefinedAt(u: Unit) = { println("0:isDefinedAt"); false }
    }
    val pf1 = new PartialFunction[Unit, Unit] {
      def apply(u: Unit): Unit = { println("1:apply") }
      def isDefinedAt(u: Unit) = { println("1:isDefinedAt"); false }
    }
    val pf2 = new PartialFunction[Unit, Unit] {
      def apply(u: Unit): Unit = { println("2:apply") }
      def isDefinedAt(u: Unit) = { println("2:isDefinedAt"); true }
    }

    val chained = pf0 orElse pf1 orElse pf2
    chained(())
  }

  def fromFunctionLiteralTest(): Unit = {
    def isEven(n: Int): Boolean = PartialFunction.cond(n)(_ % 2 == 0)
    println(isEven(1))
    println(isEven(2))
    println((1 to 5).map(_.toString))
    println((1 to 5).collect(_.toString))
  }

  def takeFunction1(fn: String => String)                      = println("fn only")
  def takePartialFunction(pf: PartialFunction[String, String]) = println("pf only")
  def takeFunctionLike(fn: String => String)                   = println("fn wins")
  def takeFunctionLike(pf: PartialFunction[String, String])    = println("pf wins")

  def testOverloadingWithFunction1(): Unit = {
    println("testing function literal syntax with methods overloaded for Function1 and PartialFunction")
    println("base case: a method that takes a Function1, so no overloading")
    takeFunction1(_.reverse)
    takeFunction1 { case s => s.reverse }
    takeFunction1 { case s: String => s.reverse }
    println()

    println("base case: a method that takes a PartialFunction, so no overloading")
    takePartialFunction(_.reverse)
    takePartialFunction { case s => s.reverse }
    takePartialFunction { case s: String => s.reverse }
    println()

    println("test case: a method that is overloaded for Function1 and PartialFunction")
    takeFunctionLike(_.reverse)
    takeFunctionLike { case s => s.reverse }
    takeFunctionLike { case s: String => s.reverse }
  }

  def reverse(s: String): String = s.reverse

  def testEtaExpansion(): Unit = {
    println("testing eta-expansion with methods overloaded for Function1 and PartialFunction")
    println("base case: a method that takes a Function1, so no overloading")
    takeFunction1(x => reverse(x))
    takeFunction1(reverse(_))
    takeFunction1(reverse _)
    takeFunction1(reverse)
    println()

    println("base case: a method that takes a PartialFunction, so no overloading")
    takePartialFunction(x => reverse(x))
    takePartialFunction(reverse(_))
    takePartialFunction(reverse _)
  //takePartialFunction(reverse)          // can't pass a method to a method that takes a PartialFunction
    println()

    println("test case: a method that is overloaded for Function1 and PartialFunction")
    takeFunctionLike(x => reverse(x))
    takeFunctionLike(reverse(_))
    takeFunctionLike(reverse _)
  //takeFunctionLike(reverse)             // can't pass a method to a method overloaded to take a Function1 or a PartialFunction
  }

  def main(args: Array[String]): Unit = {
    collectTest()
    orElseTest()
    println()

    fromFunctionLiteralTest()
    println()

    testOverloadingWithFunction1()
    println()

    testEtaExpansion()
  }
}
