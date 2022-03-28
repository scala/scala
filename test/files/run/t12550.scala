trait T extends Any {
  def i: Int

  def ti: Int = i
	def tj(y: Int)(z: Int): Int = i + y + z
  def tk(c: C): Int = i + c.i
}

class C(private val x: Int) extends AnyVal with T {
	def i: Int = x
	def j(y: Int)(z: Int): Int = x + y + z
  def k(c: C): Int = x + c.i
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C(42)
    assert(t(c) == 42)
    assert(u(c) == 42)
    assert(v(c) == 84)

    assert(tt(c) == 42)
    assert(tu(c) == 42)
    assert(tv(c) == 84)

    assert(ttt(c) == 42)
    assert(ttu(c) == 42)
    assert(ttv(c) == 84)
  }

  def t(c: C) = (try c catch { case _ :Throwable => c }).i
  def u(c: C) = (try c catch { case _ :Throwable => c }).j(-1)(1)
  def v(c: C) = (try c catch { case _ :Throwable => c }).k(try c catch { case _ :Throwable => c })

  def tt(c: C) = (try c catch { case _ :Throwable => c }).ti
  def tu(c: C) = (try c catch { case _ :Throwable => c }).tj(-1)(1)
  def tv(c: C) = (try c catch { case _ :Throwable => c }).tk(try c catch { case _ :Throwable => c })

  def ttt(c: T) = (try c catch { case _ :Throwable => c }).ti
  def ttu(c: T) = (try c catch { case _ :Throwable => c }).tj(-1)(1)
  def ttv(c: T) = (try c catch { case _ :Throwable => c }).tk(try new C(42) catch { case _ :Throwable => new C(42) })
}
