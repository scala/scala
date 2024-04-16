//> using options -Werror

trait T {

  object Nope extends Throwable

  def f(): Unit = {
    // anything but simple type tests forced analysis
    try { 1 } catch {
      case _: Exception => println("Something went wrong")
      case e: IllegalArgumentException => println(e.getMessage)
      case Nope => ???
    }

    try { 1 } catch {
      case _: Exception => println("Something went wrong")
      case e: IllegalArgumentException => println(e.getMessage)
    }

    (new IllegalArgumentException()) match {
      case _: Exception => println("Something went very wrong")
      case e: IllegalArgumentException => println(e.getMessage)
    }
  }
}
