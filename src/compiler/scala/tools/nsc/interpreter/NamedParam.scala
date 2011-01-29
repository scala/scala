/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import NamedParam._

trait NamedParamCreator {
  protected def freshName: () => String

  def apply[T: Manifest](name: String, x: T): NamedParam = new Typed[T](name, x)
  def apply[T: Manifest](x: T): NamedParam = apply(freshName(), x)

  def clazz(name: String, x: Any): NamedParam = new Untyped(name, x)
  def clazz(x: Any): NamedParam = clazz(freshName(), x)

  implicit def namedValue[T: Manifest](name: String, x: T): NamedParam = apply(name, x)
  implicit def tuple[T: Manifest](pair: (String, T)): NamedParam       = apply(pair._1, pair._2)
}

object NamedParam extends NamedParamCreator {
  class Typed[T: Manifest](val name: String, val value: T) extends NamedParam {
    val tpe = TypeStrings.fromManifest[T]
  }
  class Untyped(val name: String, val value: Any) extends NamedParam {
    val tpe = TypeStrings.fromValue(value)
  }

  protected val freshName = {
    var counter = 0
    () => { counter += 1; "p" + counter }
  }
}

trait NamedParam {
  def name: String
  def tpe: String
  def value: Any
  override def toString = name + ": " + tpe
}