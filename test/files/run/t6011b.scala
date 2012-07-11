object Test extends App {
  var cond = true

  // should not generate a switch
  def f(ch: Char): Int = ch match {
    case 'a' if cond => 1
    case 'z' | 'a' => 2
  }

  println(f('a') + f('z')) // 3
}