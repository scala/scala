/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

object NamedParam {
  def apply[T: Manifest](name: String, x: T): NamedParam[T] = new NamedParam[T](name, x)
  def apply[T: Manifest](x: T): NamedParam[T] = apply(getParamName(), x)

  implicit def fromValue[T: Manifest](x: T) = apply(x)
  implicit def fromNameAndValue[T: Manifest](name: String, x: T) = apply(name, x)
  implicit def fromTuple[T: Manifest](pair: (String, T)) = apply(pair._1, pair._2)

  private val getParamName = {
    var counter = 0
    () => { counter += 1; "p" + counter }
  }
}

class NamedParam[T: Manifest](val name: String, val value: T) {
  val clazz = manifest[T].erasure.getName
  val tparams = manifest[T].typeArguments match {
    case Nil  => ""
    case xs   => xs.mkString("[", ", ", "]")
  }
  val tpe = clazz + tparams
  override def toString = name + ": " + tpe
}
