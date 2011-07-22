class A {
  { val x = classOf[List[_]] }
  def f = {
    val g = classOf[List[_]]
    List(g, g)
  }
}