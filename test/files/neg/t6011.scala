object Test {
  def f(ch: Char): Any = ch match {
    case 'a'  => 1
    case 'a' | 'c' => 1 // unreachable
  }

  // won't be compiled to a switch since it has an unreachable (duplicate) case
  def f2(ch: Char): Any = (ch: @annotation.switch) match {
    case 'a' | 'b'  => 1
    case 'b' | 'a'  => 1 // unreachable
    case _ =>
  }

  // s'all good
  def f3(ch: Char): Any = (ch: @annotation.switch) match {
    case 'a' | 'b'  if (true: Boolean) => 1
    case 'b' | 'a'  => 1 // ok
    case _ => // need third case to check switch annotation (two-case switches are always okay to compile to if-then-else)
  }


  def main(args: Array[String]): Unit = f('a')
}