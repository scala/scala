object E extends Z {
  def X = 3
  implicit class X(val i: Int) {
    def xx = i
  }

  def Y(a: Any) = 0
  object Y
  implicit class Y(val i: String) { def yy = i }

  implicit class Z(val i: Boolean) { def zz = i }
}

trait Z {
	def Z = 0
}

object Test {
	import E._
	0.xx

	"".yy

  true.zz
}
