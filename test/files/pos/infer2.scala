module test {

  def f[a, b <: a](x: b): a = x: a;
  def g[a >: b, b](x: b): a = x: a;

  val x: int = f(1);
  val y: String = g("")

}

