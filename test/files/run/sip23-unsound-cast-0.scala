object Test extends App {
  def check[T](body: => T): Unit =
    try {
      body
      println("OK")
    } catch {
      case thr: Throwable =>
        println(thr.getClass.getName)
    }

  trait Global
  final val global0 = new Global {}
  final val global1 = new Global {}

  check(global0.asInstanceOf[global0.type])
  check(global0.asInstanceOf[global1.type])
}
