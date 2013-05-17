import scala.reflect.macros.Context

object Macros {
  def impl(c: Context) = c.universe.reify {
    println("enclosingPackage = " + c.literal(c.enclosingPackage.toString).splice)
    println("enclosingClass = " + c.literal(c.enclosingClass.toString).splice)
    println("enclosingImpl = " + c.literal(c.enclosingImpl.toString).splice)
    println("enclosingTemplate = " + c.literal(c.enclosingTemplate.toString).splice)
    println("enclosingMethod = " + c.literal(c.enclosingMethod.toString).splice)
    println("enclosingDef = " + c.literal(c.enclosingDef.toString).splice)
  }

  def foo = macro impl
}