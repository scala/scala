class Foo[A]

class Test {
  def append(a: Foo[String]): Unit = ()
  def append(a: Foo[String], as: Int*): Unit = ()

  append(new Foo)
}
