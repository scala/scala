trait X {
  val elem: Int = 1
}

module test {

  def g(x: X) = x.elem;
  def f(x: Object) = x.toString();

}
