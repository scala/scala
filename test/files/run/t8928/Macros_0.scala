package test

object Macros {
  import language.experimental.macros
  def apply[T]: Unit = macro impl[T]

  import reflect.macros.whitebox
  def impl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    new Checks[c.universe.type](c.universe, ordered = true)
      .check(weakTypeOf[T])

    Literal(Constant(()))
  }
}