class C(val x: String) extends AnyVal

object TC {
  def c(s: String): C = new C(s)
  def p = 2
  def test(v: Int, w: Int): Any = 
    v match {
      case 1 if p == w => c("a")
      case _ => c("b")
    }
}

object TI {
  def c(i: Int): Int = i
  def p = 2
  def test(v: Int, w: Int): Any = 
    v match {
      case 1 if p == w => c(42)
      case _ => c(43)
    }
}

object Test extends App {
  assert(TC.test(1, 2).asInstanceOf[C].x == "a")
  assert(TC.test(1, 3).asInstanceOf[C].x == "b")
  assert(TC.test(2, 2).asInstanceOf[C].x == "b")
  assert(TI.test(1, 2) == 42)
  assert(TI.test(1, 3) == 43)
  assert(TI.test(2, 2) == 43)
}