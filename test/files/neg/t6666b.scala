class C(a: Any)
object F {
  def byname(a: => Any) = println(a)
  def hof(a: () => Any) = println(a())
}


class C5 extends C({
  def x = "".toString
  val y = {
    object Nested { def xx = x}
    Nested.xx
  }
})


class C15(a: Any) {
  def this() = {
    this({
      def x = "".toString
      val y = {
        object Nested { def xx = x}
        Nested.xx
      }
    })
  }
}
