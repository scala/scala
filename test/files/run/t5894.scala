import language.experimental.macros

class Test

object Test {
  def foo = macro fooImpl
  def fooImpl(c: reflect.macros.Context) = c.literalUnit

  def main(args: Array[String]) {
    try {
      val method = classOf[Test].getMethod("foo")
      sys.error("Static forwarder generated for macro: " + method)
    } catch {
      case _: NoSuchMethodException => // okay
    }
  }
}
