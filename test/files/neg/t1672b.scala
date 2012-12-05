object Test {
  @annotation.tailrec
  def bar : Nothing = {
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => bar
    } finally {
      bar
    }
  }

  @annotation.tailrec
  def baz : Nothing = {
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => baz
    } finally {
      ???
    }
  }

  @annotation.tailrec
  def boz : Nothing = {
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => boz; ???
    }
  }

  @annotation.tailrec
  def bez : Nothing = {
    try {
      bez
    } finally {
      ???
    }
  }

  // the `liftedTree` local method will prevent a tail call here.
  @annotation.tailrec
  def bar(i : Int) : Int = {
    if (i == 0) 0
    else 1 + (try {
      throw new RuntimeException
    } catch {
      case _: Throwable => bar(i - 1)
    })
  }
}
