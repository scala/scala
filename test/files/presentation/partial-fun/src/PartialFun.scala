class A {
  def foo {
    val x: PartialFunction[Int, Int] = ({ case 0 => 0 })
  }
}
