
trait P {
  def p: String = "p"
}

object X extends P {
  override final val p = "x"
}

// was: Exception in thread "main" java.lang.ClassFormatError: Duplicate method name "p" with signature "()Ljava.lang.String;" in class file X$
object Test {
  def main(args: Array[String]): Unit = X
}
