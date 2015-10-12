import language.experimental.macros


object Macro {

  import reflect.macros.blackbox.Context
  def impl(c: Context)(): c.Tree = {
    import c.universe._
    val tree = q"""class C; new C"""
    val tree1 = c.typecheck(tree)
    val tpe = tree1.tpe
    val tree2 = c.typecheck(c.untypecheck(tree1))
    q"""$tree2.asInstanceOf[$tpe]"""
  }
  def apply(): Any = macro impl
}
