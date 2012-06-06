package scala.reflect.reify
package codegen

trait GenNames {
  self: Reifier =>

  import global._
  import definitions._

  def reifyName(name: Name) = {
    val factory = if (name.isTypeName) nme.nmeNewTypeName else nme.nmeNewTermName
    mirrorCall(factory, Literal(Constant(name.toString)))
  }
}