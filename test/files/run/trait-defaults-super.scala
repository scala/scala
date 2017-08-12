trait T extends java.lang.Iterable[String] {

  override def spliterator(): java.util.Spliterator[String] = {
    super[Iterable].spliterator
    super.spliterator
    null
  }
  def foo = {
    super[Iterable].spliterator
    super.spliterator
  }
  def iterator(): java.util.Iterator[String] = java.util.Collections.emptyList().iterator()
}
class C extends T with java.lang.Iterable[String] // super accessor is okay with Iterable as a direct parent
object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    c.spliterator
    c.foo
    val t: T = c
    t.spliterator
    t.foo
  }
}
