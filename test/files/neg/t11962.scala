//> using options -Xlint -Werror -Ystop-after:refchecks
trait T extends Any {
  def f = println()
}

class C(val x: Any) extends AnyVal with T {
  override def f = super.f
}
