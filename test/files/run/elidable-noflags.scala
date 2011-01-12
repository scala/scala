import annotation._
import elidable._

object Test {
  @elidable(FINEST) def f1() = println("Good for me, I was not elided.")
  @elidable(INFO) def f2()   = println("Good for me, I was not elided.")
  @elidable(SEVERE) def f3() = println("Good for me, I was not elided.")
  @elidable(INFO) def f4()   = println("Good for me, I was not elided.")
  @elidable(100000) def f5() = println("Good for me, I was not elided.")
  @elidable(OFF) def f6() = println("Good for me, I was not elided.")
  @elidable(ALL) def f7() = println("ESPECIALLY good for me, I was not elided.")

  def main(args: Array[String]): Unit = {
    f1()
    f2()
    f3()
    f4()
    f5()
    f6()
    f7()
  }
}
