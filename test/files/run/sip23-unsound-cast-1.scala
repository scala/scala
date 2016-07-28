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

  check(0.asInstanceOf[0])
  check(1.asInstanceOf[0])
  check(0L.asInstanceOf[0L])
  check(1L.asInstanceOf[0L])
  check(0.0.asInstanceOf[0.0])
  check(1.0.asInstanceOf[0.0])
  check(0.0F.asInstanceOf[0.0F])
  check(1.0F.asInstanceOf[0.0F])
  check(true.asInstanceOf[true])
  check(true.asInstanceOf[false])
  check('f'.asInstanceOf['f'])
  check('f'.asInstanceOf['b'])
  check("foo".asInstanceOf["foo"])
  check("foo".asInstanceOf["bar"])
  check('foo.asInstanceOf['foo])
  check('foo.asInstanceOf['bar])
}
