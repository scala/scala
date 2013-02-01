object Test {
  @annotation.tailrec
  def bar : Nothing = {
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => bar
    }
  }
}
