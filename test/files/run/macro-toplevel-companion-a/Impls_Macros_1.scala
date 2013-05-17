import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val synthetic = reify{ class C { override def toString = "C" }; object C { implicit val c = new C } }.tree
    val defs = synthetic.asInstanceOf[Block].stats.asInstanceOf[List[ImplDef]]
    if (c.topLevelRef(TypeName("C")).isEmpty) c.introduceTopLevel(nme.EMPTY_PACKAGE_NAME.toString, defs: _*)
    c.literalUnit
  }

  def foo = macro impl
}