trait P {
  def p: String = "p"
}

object X extends P {
  override final val p = "x"
}

object X2 extends P {
  override final val p: "x" = "x"
}

trait Q {
  def q: Int = 27
}

object Y extends Q {
  override final val q = 42
}

object Y2 extends Q {
  override final val q: 42 = 42
}

class K {
  def k: Class[_] = classOf[K]
}

object L extends K {
  override final val k = classOf[L.type]
}

// was: Exception in thread "main" java.lang.ClassFormatError: Duplicate method name "p" with signature "()Ljava.lang.String;" in class file X$
object Test extends App {
  (X, Y, L)
}
