
//> using options -Xsource:3-cross

trait T {
  def f(c: Char) = raw"\u%04X".format(c.toInt)
}
