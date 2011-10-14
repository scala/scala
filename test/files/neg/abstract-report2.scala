import java.util.Collection

class Foo extends Collection[Int]

class Bar extends Collection[List[_ <: String]]

class Baz[T] extends Collection[T]

trait Bippy[T1, T2, T3] extends Collection[T1] with TraversableOnce[(T2, String)] with Ordering[T3]

class Dingus extends Bippy[String, Set[Int], List[Int]]