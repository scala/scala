object T {
  val f: Unit = () => ()
  println(f)
}

object U {
  def f[T](t: T): T = t
  f[Unit](() => ())
}

object Test {
  def main(args: Array[String]): Unit = {
    T
    U
  }
}
