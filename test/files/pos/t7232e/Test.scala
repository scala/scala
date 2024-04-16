//> using options -Werror
object Test {
  import pack._

  // java.util.List not java.awt.List or pack.List
  Foo.list().listIterator()
}
