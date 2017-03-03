/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.language.implicitConversions

import NamedParam._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.{ClassTag}
import scala.tools.nsc.typechecker.{ TypeStrings }

trait NamedParamCreator {
  protected def freshName: () => String

  def apply[T: ru.TypeTag : ClassTag](name: String, x: T): NamedParam = new Typed[T](name, x)
  def apply[T: ru.TypeTag : ClassTag](x: T): NamedParam = apply(freshName(), x)
  def clazz(name: String, x: Any): NamedParam = new Untyped(name, x)

  implicit def tuple[T: ru.TypeTag : ClassTag](pair: (String, T)): NamedParam       = apply(pair._1, pair._2)
}

object NamedParam extends NamedParamCreator {
  class Typed[T: ru.TypeTag : ClassTag](val name: String, val value: T) extends NamedParam {
    val tpe = TypeStrings.fromTag[T]
  }
  class Untyped(val name: String, val value: Any) extends NamedParam {
    val tpe = TypeStrings.fromValue(value)
  }

  protected val freshName = {
    var counter = 0
    () => { counter += 1; "p" + counter }
  }
}

case class NamedParamClass(name: String, tpe: String, value: Any) extends NamedParam { }

trait NamedParam {
  def name: String
  def tpe: String
  def value: Any
  override def toString = name + ": " + tpe
}
