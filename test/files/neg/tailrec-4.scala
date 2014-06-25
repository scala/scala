import annotation._

object Tail {
  def tcInFunc: Unit = {
    () => {
      @tailrec def foo: Int = foo + 1
    }
  }
  def tcInBooleanExprFirstOp(x: Int, v: Int): Boolean = {
    {
      @tailrec def foo: Int = foo + 1
      foo
    } == v && true
  }
  def tcInBooleanExprSecondOp(x: Int, v: Int): Boolean = {
    true && {
      @tailrec def foo: Int = foo + 1
      foo
    } == v
  }
  def tcInIfCond(x: Int, v: Int): Boolean = {
    if ({
      @tailrec def foo: Int = foo + 1
      foo
    } == v) true else false
  }
  def tcInPatternGuard(x: Int, v: Int): Boolean =
    v match {
      case _ if
        {
          @tailrec def foo: Int = foo + 1
          foo == 42
        } => true
    }
}
