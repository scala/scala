trait DefPwS {
  def get(list: Boolean) = if (list) List(1, 2, 3) else (1, 2, 3) // warn
}

trait ValPwS {
  val foo = if (true) List(1, 2) else (1, 2) // warn
}

trait ParamPwS {
  def f[A](as: A*) = 42
  val g = f((1, 2), List(1, 2)) // warn
}

trait GenericTraitPwS[+A] {
  { List(List(1, 2)) contains ((1, 2)) } // warn
}

// these should not warn as they have explicit types
trait NoWarning {
  def get(list: Boolean): Product with Serializable =
    if (list) List(1, 2) else (1, 2)
  lazy val foo: Product with Serializable = if (true) List(1, 2) else (1, 2)
  lazy val bar: Any = if (true) List(1, 2) else (1, 2)
  def f[A](as: A*) = 42
  lazy val baz = f[Product with Serializable]((1, 2), List(1, 2))
  def g[A >: Product with Serializable](as: A*) = 42
  lazy val biz = g((1, 2), List(1, 2))
}
