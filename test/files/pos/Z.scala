trait X {
  val elem: Int = 1
}

object test {

  def g(x: X) = x.elem;
  def f(x: AnyRef) = x.toString();

}
