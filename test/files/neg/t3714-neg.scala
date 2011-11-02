// this is a slight negative twist on run/t3714.scala.
trait Break {
  protected val break: Int;
}

class BreakImpl(protected val break: Int) extends Break { }
object BreakImpl {
  def apply(x: Int): Break = new BreakImpl(x)
  def unapply(x: Any) = x match {
    case x: BreakImpl => Some(x.break)
    case _            => None
  }
}

object Test {
  def f1(x: Break) = x match {
    case b: BreakImpl => b.break
    case b            => -1
  }
  def f2(x: Break) = x match {
    case BreakImpl(x) => x
    case _            => -1
  }
  def f3(x: Any) = x match {
    case b: BreakImpl => b.break
    case b            => -1
  }
  def f4(x: Any) = x match {
    case BreakImpl(x) => x
    case _            => -1
  }
  
  def main(args: Array[String]) {
    val break = BreakImpl(22)
    assert(f1(break) == 22)
    assert(f2(break) == 22)
    assert(f3(break) == 22)
    assert(f4(break) == 22)
  }
}

