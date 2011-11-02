trait Break {
  protected val break: Int;
}

case class BreakImpl(protected val break: Int) extends Break { }

object Test {
  // def f1(x: Break) = x match {
  //   case b: BreakImpl => b.break
  //   case b            => -1
  // }
  def f2(x: Break) = x match {
    case BreakImpl(x) => x
    case _            => -1
  }
  // def f3(x: Any) = x match {
  //   case b: BreakImpl => b.break
  //   case b            => -1
  // }
  def f4(x: Any) = x match {
    case BreakImpl(x) => x
    case _            => -1
  }
  
  def main(args: Array[String]) {
    val break = BreakImpl(22)
    // assert(f1(break) == 22)
    assert(f2(break) == 22)
    // assert(f3(break) == 22)
    assert(f4(break) == 22)
  }
}

