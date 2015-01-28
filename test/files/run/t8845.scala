// crashes compiler under GenASM, works under GenBCode.
object Interpreter {
  def mkDataProp(i: Int) = i
  def break(n: Int): Unit =
    try {
      n match {
        case _ =>
          val newDesc = mkDataProp(n)
          n match { case _ => return }
      }
    } catch { case e: Throwable => }
    finally { }
}

object Test extends App {
  Interpreter.break(0)
}
