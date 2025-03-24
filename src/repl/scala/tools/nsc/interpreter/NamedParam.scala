/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.interpreter

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}
import scala.tools.nsc.typechecker.TypeStrings

trait NamedParamCreator {
  import NamedParam._
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

trait NamedParam {
  def name: String
  def tpe: String
  def value: Any
  override def toString = name + ": " + tpe
}

case class NamedParamClass(name: String, tpe: String, value: Any) extends NamedParam
