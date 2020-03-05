import java.util.Collection

class Foo extends Collection[Int]

class Bar extends Collection[List[_ <: String]]

class Baz[T] extends Collection[T]

trait Xyz[T] {
  def foo(x: T): Boolean
}

trait Symbolic {
  def --? : Int
  def --!(i: Int): Unit
  def unary_~ : Long
}

trait Bippy[T1, T2, T3] extends collection.IterableOps[(T2, String), List, List[(T2, String)]] with Xyz[T3]

class Dingus extends Bippy[String, Set[Int], List[Int]]
