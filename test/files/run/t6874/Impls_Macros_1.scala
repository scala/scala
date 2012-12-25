import language.experimental.macros
import scala.reflect.macros.Context

trait Mock {
  import language.experimental.macros

  type mock[T](implicit x: Int) = macro MockImpl.mock[T]
}

object MockImpl {
  import reflect.macros.Context

  def mock[T: c.WeakTypeTag](c: Context)(x: c.Expr[Int]): c.Tree = {
    import c.universe._

    val typeToMock = weakTypeOf[T]

    def getPackage(sym: Symbol): RefTree =
      if (sym.owner == c.mirror.RootClass)
        Ident(sym.name.toTermName)
      else
        Select(getPackage(sym.owner), sym.name.toTermName)

    val mockName = c.freshName(typeToMock.typeSymbol.name).toTypeName
    val mockPackage = getPackage(typeToMock.typeSymbol.owner)

    val classDef = ClassDef(NoMods, mockName, Nil, Template(Nil, emptyValDef, List(DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(())))))))

    c.introduceTopLevel(mockPackage.toString, classDef)
    Select(mockPackage, mockName)
  }
}