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
class C extends T
object Test {
  def main(args: Array[String]): Unit = {
    val t: T = new C
    t.spliterator
    t.foo
  }
}
