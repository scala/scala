object Test {
  def spec[@specialized(Byte, Short, Int, Long) T : Integral](t: T) = {
    def inner(default: T = t): T = t
    inner()
  }
}
