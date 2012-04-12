package scala.reflect.reify
package codegen

trait Names {
  self: Reifier =>

  import mirror._
  import definitions._
  import treeInfo._

  def reifyName(name: Name) = {
    val factory = if (name.isTypeName) nme.nmeNewTypeName else nme.nmeNewTermName
    mirrorCall(factory, Literal(Constant(name.toString)))
  }
}