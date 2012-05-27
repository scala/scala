// There are two versions of this tests: one with and one without specialization.
// The bug was only exposed *without* specialization.
trait T extends Any {
  def x: Any
}

final class StringOps(val repr0: String) extends AnyVal with T {
  def x = ()
}
