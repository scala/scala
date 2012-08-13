// this test checks if effectively final methods are inlined
// for traits grep for SI-4767
class Base {
  @inline def foo(n: Int) = n
}

object Test extends Base {
  def main(args: Array[String]) = {
    println(foo(42))
  }
}
