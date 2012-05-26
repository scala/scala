trait T extends Any {
  def x: Any
}

final class StringOps(val repr0: String) extends AnyVal with T {
  def x = ()
}
