import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context) = {
    import c.universe._
    def chain(sym: Symbol): List[Symbol] = sym.owner match {
      case NoSymbol => sym :: Nil
      case owner => sym :: chain(owner)
    }
    q"""
      println("enclosingPackage = " + ${c.enclosingPackage.toString})
      println("enclosingClass = " + ${c.enclosingClass.toString})
      println("enclosingImpl = " + ${c.enclosingImpl.toString})
      println("enclosingTemplate = " + ${c.enclosingTemplate.toString})
      println("enclosingMethod = " + ${c.enclosingMethod.toString})
      println("enclosingDef = " + ${c.enclosingDef.toString})
      println("enclosingOwner = " + ${c.internal.enclosingOwner.toString})
      println("enclosingOwnerChain = " + ${chain(c.internal.enclosingOwner).toString})
    """
  }

  def foo: Any = macro impl
}