object Test {
  def foo(f: Int => Int) = ()
  def test(): Unit = {
    foo { x => x + 1 }
    foo { implicit x => x + 1 }
    foo { x: Int => x + 1 }
    foo { implicit x: Int => x + 1 }
    foo { _ => 42 }
    foo { implicit _ => implicitly[Int] + 1 }   // scala 2 deficit
    foo { _: Int => 42 }
    foo { implicit _: Int => implicitly[Int] + 1 }    // scala 2 deficit

    foo(x => x + 1)
    foo(implicit x => x + 1)
    foo((x: Int) => x + 1)
    foo(implicit (x: Int) => x + 1)   // scala 3
    foo(_ => 42)
    foo(implicit _ => implicitly[Int] + 1)    // scala 2 deficit
    foo((_: Int) => 42)
    foo(implicit (_: Int) => implicitly[Int] + 1)   // scala 3
  }
}
