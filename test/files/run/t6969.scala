

import scala.language.{ reflectiveCalls }

object Test {
  private type Clearable = { def clear(): Unit }
  private def choke() = {
    try new Array[Object]((Runtime.getRuntime().maxMemory min Int.MaxValue).toInt)
    catch {
      case _: OutOfMemoryError => // what do you mean, out of memory?
      case t: Throwable        => println(t)
    }
  }
  private def f(x: Clearable) = x.clear()
  class Choker(id: Int) extends Thread {
    private def g(iteration: Int) = {
      val map = scala.collection.mutable.Map[Int, Int](1 -> 2)
      try f(map) catch { case t: NullPointerException => println(s"Failed at $id/$iteration") ; throw t }
      choke()
    }
    override def run() {
      1 to 50 foreach g
    }
  }

  def main(args: Array[String]): Unit = {
    val threads = 1 to 3 map (id => new Choker(id))
    threads foreach (_.start())
    threads foreach (_.join())
    println("All threads completed.")
  }
}
