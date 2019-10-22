package tastytest

object Extensions {
  implicit class AddHello[A](private val a: A) extends AnyVal {
    def hello: String = "Hello"
  }
}
