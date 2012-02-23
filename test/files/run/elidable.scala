import annotation._
import elidable._

object Test {
  @elidable(FINEST) def f1() = assert(false, "Should have been elided.")
  @elidable(INFO) def f2() = assert(false, "Should have been elided.")
  @elidable(SEVERE) def f3() = println("Good for me, I was not elided.")
  @elidable(INFO) def f4 = assert(false, "Should have been elided (no parens).")
  
  def main(args: Array[String]): Unit = {
    f1()
    f2()
    f3()
    f4
  }
}
