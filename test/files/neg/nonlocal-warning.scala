class Foo {
  def foo(l: List[Int]): Int = {
    try l foreach { _ => return 5 }
    catch { case x => 11 }
    22
  }
}
