import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    val x1 = c.Expr[Int](c.resetAllAttrs(x.tree))
// was:    c.literal(x1.splice)
    c.literal(eval(c)(x1))
  }

  private def eval[T](c: Ctx)(x: c.Expr[T]): T = {
    import scala.reflect.runtime.{universe => ru}
    val mirror = ru.runtimeMirror(c.libraryClassLoader)
    import scala.tools.reflect.ToolBox
    val toolBox = mirror.mkToolBox()
    val importer = ru.mkImporter(c.universe).asInstanceOf[ru.Importer { val from: c.universe.type }]
    val tree = c.resetAllAttrs(x.tree.duplicate)
    val imported = importer.importTree(tree)
    toolBox.runExpr(imported).asInstanceOf[T]
  }
}

object Macros {
  def foo(x: Int) = macro Impls.foo
}
