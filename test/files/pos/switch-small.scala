import annotation._

object Test {
  def f(x: Int) = (x: @switch) match {
    case 1 => 1
    case _ => 2
  }
}
