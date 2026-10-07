
class JImpl extends J {
  override def f(e: J.E): Unit = ()
}

object Test extends App {
  new JImpl
}
