object Test {
  trait Immortal
  class Bippy extends Immutable with Immortal
  class Boppy extends Immutable
  
  def f[T](x: Traversable[T]) = x match {
    case _: Map[_, _]   => 3
    case _: Seq[_]      => 2
    case _: Iterable[_] => 1
    case _              => 4
  }
  def g(x: Bippy) = x match {
    case _: Immutable with Immortal => 1
    case _                          => 2
  }
  def h(x: Immutable) = x match {
    case _: Immortal => 1
    case _           => 2
  }

  def main(args: Array[String]): Unit = {
    println(f(Set(1)))
    println(f(Seq(1)))
    println(f(Map(1 -> 2)))
    println(f(null))
    
    println(g(new Bippy))
    println(g(null))
    
    println(h(new Bippy))
    println(h(new Boppy))
    println(h(null))
  }
}
