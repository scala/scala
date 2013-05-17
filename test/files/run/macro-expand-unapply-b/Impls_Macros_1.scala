import language.experimental.macros
import scala.reflect.macros.Context

object Macros {
  implicit class ContextExtensions(c: StringContext) {
    object q {
      def unapply(x: Any): Option[Any] = macro impl
    }
  }

  def impl(c: Context)(x: c.Expr[Any]): c.Expr[Option[Any]] = {
    import c.universe._
    import Flag._

    // parts here will be string literals - static parts of the string interpolation
    // e.g. for q"$x, $y" parts will be Literal(Constant("")), Literal(Constant(", ")) and Literal(Constant(""))
    val Apply(Select(Select(Apply(_, List(Apply(_, parts))), _), _), _) = c.macroApplication
    val nresults = parts.length - 1

    def results() =
      ((1 to (nresults - 1)).toList map (i => Literal(Constant(i)))) :+  // (n - 1) results of type Int
      Apply(Ident(TermName("List")), List(Literal(Constant(nresults))))  // and also one result of a different type
    def extractorBody() =
      if (nresults == 0) Literal(Constant(true))
      else if (nresults == 1) Apply(Ident(TermName("Some")), results())
      else Apply(Ident(TermName("Some")), List(Apply(Ident(TermName("Tuple" + nresults)), results())))

    val name = TermName(java.util.UUID.randomUUID().toString.replace("-", ""))
    val mdef = ModuleDef(NoMods, name, Template(List(Select(Ident(TermName("scala")), TypeName("AnyRef"))), emptyValDef, List(
      DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
        Block(List(pendingSuperCall), Literal(Constant(())))),
      DefDef(Modifiers(), TermName("unapply"), List(), List(List(ValDef(Modifiers(PARAM), TermName("x"), Ident(TypeName("Any")), EmptyTree))), TypeTree(),
        extractorBody()))))
    c.introduceTopLevel(nme.EMPTY_PACKAGE_NAME.toString, mdef)
    c.Expr[Option[Any]](Apply(Select(Ident(name), TermName("unapply")), List(x.tree)))
  }
}