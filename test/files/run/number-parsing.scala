object Test {
  def numTests() = {
    val MinusZero = Float.box(-0.0f)
    val PlusZero  = Float.box(0.0f)
  
    assert(PlusZero match { case MinusZero => false ; case _ => true })
    assert(MinusZero match { case PlusZero => false ; case _ => true })
    assert((MinusZero: scala.Float) == (PlusZero: scala.Float))
    assert(!(MinusZero equals PlusZero))
  
    List(
      -5f.max(2) ,
      -5f max 2 ,
      -5.max(2) ,
      -5 max 2
    ) foreach (num => assert(num == 2))
  }

  case class Foo(val x: Double) {
    def unary_- : Foo = Foo(-x)
    def +(other: Foo): Foo = Foo(x + other.x)
  }
  def objTests = {
    assert(-Foo(5.0) + Foo(10.0) == Foo(5.0))
    assert(-Foo(5.0).+(Foo(10.0)) == Foo(-15.0))
  }

  def main(args: Array[String]): Unit = {
    numTests()
  }
}
