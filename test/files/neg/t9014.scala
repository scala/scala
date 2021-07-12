object Test {
  def spec[@specialized(Byte, Short, Int, Long) T : Integral](t: T) = {
    // still broken - specialize can't deal with the synthetic companion object
    case class Inner(default: T)
    t
  }
}
