object Test {
  def f(ch: Char): Any = ch match {
    case 'a'  => 1
    case 'a' | 'c' => 1 // unreachable
  }
  def main(args: Array[String]): Unit = f('a')
}