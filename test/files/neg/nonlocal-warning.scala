class Foo {
  def foo(l: List[Int]): Int = {
    try l foreach { _ => return 5 }
    catch { case x => 11 }
    22
  }

  val pf: PartialFunction[Throwable, Unit] = {
    case x if false => ()
  }

  def bar(l: List[Int]): Int = {
    try l foreach { _ => return 5 }
    catch pf
    finally println()
    22
  }
}
