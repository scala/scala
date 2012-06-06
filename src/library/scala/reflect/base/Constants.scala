/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package base

trait Constants {
  self: Universe =>

  type Constant >: Null <: AnyRef
  implicit val ConstantTag: ClassTag[Constant]
  val Constant: ConstantExtractor

  abstract class ConstantExtractor {
    def apply(value: Any): Constant
    def unapply(arg: Constant): Option[Any]
  }
}
