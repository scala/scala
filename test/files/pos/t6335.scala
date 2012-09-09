object E {
  def X = 3
  implicit class X(val i: Int) {
    def xx = i
  }
}

object Test {
	import E._
	0.xx
}
