
trait Writer[A] {
  def write(value: A): String
}

object Writer {
  implicit val stringWriter: Writer[String] = new Writer[String] {
    override def write(value: String): String = value
  }
}

object Foo {
  implicit val string2foo: Writer[String] = new Writer[String] {
    override def write(value: String): String = "foo"
  }
}

object Test extends App {
  import Foo._

  implicit class WriterOps[A](value: A) {
    def toWrappedString(implicit w: Writer[A]): String = w.write(value)
  }

 //object Foo has the implicit with the same name
  implicit val string2foo: Writer[String] = new Writer[String] {
    override def write(value: String): String = "foo"
  }

  val foo = "Hello".toWrappedString
  assert(foo == "foo") //prints "foo"
  assert("Hello".toWrappedString == "foo") //prints "Hello"
}
