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

package scala.tools.partest.nest

import scala.language.implicitConversions
import scala.util.chaining._

/** This trait works together with others in scala.tools.cmd to allow
 *  declaratively specifying a command line program, with many attendant
 *  benefits.  See scala.tools.cmd.DemoSpec for an example.
 */
trait Spec {
  def referenceSpec: Reference
  def programInfo: Spec.Info

  protected def help(str: => String): Unit
  protected def heading(str: => String): Unit = help(s"\n  $str")

  type OptionMagic <: Opt.Implicit
  protected implicit def optionMagicAdditions(s: String): OptionMagic
}

object Spec {
  class Info(
    val runner: String,
    val usage: String,
    val mainClass: String
  )
  object Info {
    def apply(runner: String, help: String, mainClass: String): Info = new Info(runner, help, mainClass)
  }

  class Accumulator[T: FromString]() {
    private var _buf: List[T] = Nil

    def convert(s: String)    = implicitly[FromString[T]] apply s
    def apply(s: String): T   = convert(s).tap(_buf +:= _)

    lazy val get = _buf
  }

  class Choices[T: FromString](val xs: List[T]) {
    def fs: FromString[T] = implicitly[FromString[T]]
    def contains(x: T)    = xs contains x
    override def toString = xs.mkString("{ ", ", ", " }")
  }

  class EnvironmentVar(val name: String) {
    override def toString = s"$${$name}"
  }
}
