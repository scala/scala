import util._

import control.Exception._

object Test {
  def main(args: Array[String]): Unit = {
    println(catching(classOf[NumberFormatException]) withTry ("Hi".toDouble))
    println(catching(classOf[NumberFormatException]) withTry ("5".toDouble))
    try {
      catching(classOf[NumberFormatException]) withTry (sys.error("O NOES"))
    } catch {
       case t: Throwable => println(t.getMessage)
    }
    println(nonFatalCatch withTry ("Hi".toDouble))
  }
}
