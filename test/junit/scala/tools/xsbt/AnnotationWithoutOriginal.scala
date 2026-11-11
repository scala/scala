package scala.tools.xsbt

/**
 * Sources in which `C.f` carries an annotation with no typed original tree, as for one
 * added by a macro or a compiler plugin rather than by the typer. The macro is compiled
 * in a first run, the rest in a second.
 */
object AnnotationWithoutOriginal {
  val macroSrc: String =
    """|import scala.language.experimental.macros
       |import scala.reflect.macros.blackbox.Context
       |object Tag {
       |  def tag: Unit = macro impl
       |  def impl(c: Context): c.Expr[Unit] = {
       |    val g = c.universe.asInstanceOf[scala.tools.nsc.Global]
       |    val owner = c.internal.enclosingOwner.asInstanceOf[g.Symbol]
       |    val annTpe = g.rootMirror.staticClass("MyAnn").tpe
       |    val arg = c.typecheck(c.parse("Consts.I")).asInstanceOf[g.Tree]
       |    owner.addAnnotation(g.AnnotationInfo(annTpe, List(arg), Nil))
       |    c.Expr[Unit](c.universe.Literal(c.universe.Constant(())))
       |  }
       |}""".stripMargin

  val srcs: List[String] = List(
    "object Consts { val I = 1 }",
    "class MyAnn(x: Int) extends scala.annotation.StaticAnnotation",
    "class C { def f: Int = { Tag.tag; 1 } }",
  )

  val groupedSrcs: List[List[String]] = List(List(macroSrc), srcs)
}
