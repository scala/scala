object Test extends App {
  val f = () => 5
  def test(g: => Int) {
    val gFunc = g _
    val isSameClosureClass = gFunc.getClass == f.getClass
    val isSame = gFunc eq f
    println("Is same closure class: "+isSameClosureClass+" is same closure: "+isSame)
  }

  test(f())
}
