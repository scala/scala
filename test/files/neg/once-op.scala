// scalac: -Werror -Xlint:deprecation
//
object Foo {
  val vec = (1 to 10).toVector
  val it = vec.iterator
  println(it.take(2))
  println(it.hasNext)
  println(it.take(2))

  def foo[A1](a: scala.collection.Seq[A1], m0: Int, m1: Int, b: scala.collection.Seq[A1]): Int = {
    if (a.iterator.slice(m0, m1).sameElements(b.iterator.slice(m0, m1))) m0
    else -1
  }
  def bar[A1](m0: Int, m1: Int): Int = {
    if (List(1).iterator.slice(m0, m1).sameElements(List(2).iterator.slice(m0, m1))) m0
    else -1
  }
  def list: Iterator[Int] = ???
  def baz: Int = {
    list collect { case 0 => 1 }
    list collect { case 0 => 1 }
    0
  }
}
