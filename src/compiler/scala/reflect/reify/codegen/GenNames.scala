/* NSC -- new Scala compiler
 * Copyright 2012-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect.reify
package codegen

trait GenNames {
  self: Reifier =>

  import global._

  def reifyName(name: Name) = {
    val factory = if (name.isTypeName) nme.TypeName else nme.TermName
    mirrorCall(factory, Literal(Constant(name.toString)))
  }
}
