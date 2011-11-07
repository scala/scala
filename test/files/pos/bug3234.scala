trait Trait1 {
  // need more work before this one works
  // @inline 
  def foo2(n: Int) = n*n
}

trait Trait2 {
  @inline def foo3(n: Int) = 1
}

class Base extends Trait1 {
  @inline def foo(n: Int) = n
}

object Test extends Base with Trait2 {
  def main(args: Array[String]) = {
    println(foo(42) + foo2(11) + foo3(2))
  }
}