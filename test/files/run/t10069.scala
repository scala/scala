object Expected extends Exception
object Test {
  def throwExpected: Nothing = throw Expected
  def foo0(a: Array[Double]) = {  // does compile for Int instead of Double
    val v = 42
    a(0) = throwExpected // was crash in code gen: java.lang.NegativeArraySizeException
  }

  def foo1(a: Array[Double]) = {  // does compile for Int instead of Double
    a(0) = throwExpected // was VerifyError at runtime
  }

  def foo2(a: Array[Int]) = {  // does compile for Int instead of Double
    a(0) = throwExpected // was VerifyError at runtime
  }

  def foo3(a: Array[String]) = {  // does compile for Int instead of Double
    a(0) = throwExpected // was already working
  }


  def main(args: Array[String]): Unit = {
    check(foo0(new Array[Double](1)))
    check(foo1(new Array[Double](1)))
    check(foo2(new Array[Int](1)))
    check(foo3(new Array[String](1)))
  }
  def check(f: => Any) {
    try {f ; sys.error("no exception thrown") 
    } catch {
      case Expected =>
    } 
  }
}
