class A {
  def foo: Unit = {
    val x: PartialFunction[Int, Int] = ({ case 0 => 0 })
  }
}
