trait C {
  type T;
  def f(x: T): unit;
}

trait D extends C {
  def f(x: T): unit = super.f(x);
}
