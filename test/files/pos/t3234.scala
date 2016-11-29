trait Trait1 {
  @inline final def foo2(n: Int) = n*n
}

trait Trait2 {
  @inline final def foo3(n: Int) = 1
}

class Base extends Trait1 {
  @inline final def foo(n: Int) = n
}

object Test extends Base with Trait2 {
  def main(args: Array[String]) = {
    println(foo(42) + foo2(11) + foo3(2))
  }
}
