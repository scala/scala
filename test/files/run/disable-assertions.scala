
object Elided {
  import annotation._, elidable._
  @elidable(INFO) def info(): Boolean = true
  @elidable(10000) def f(): Boolean = true
  def g(): Boolean = { assert(false); true }
}

object Test extends App {
  import Elided._
  if (info()) println("Bad info.")
  if (!f()) println("Elided f.")
  if (!g()) println("Elided g?")   // assert should be off
}
