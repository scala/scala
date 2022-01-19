/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

// GENERATED CODE: DO NOT EDIT.
// genprod generated these sources at: 2022-01-17T20:47:12.170348200Z

package scala


/** A function of 0 parameters.
 *  
 *  In the following example, the definition of `greeting` is
 *  shorthand, conceptually, for the anonymous class definition
 *  `anonfun0`, although the implementation details of how the
 *  function value is constructed may differ:
 *
 *  {{{
 *  object Main extends App {
 *    val name = "world"
 *    val greeting = () => s"hello, $name"
 *
 *    val anonfun0 = new Function0[String] {
 *      def apply(): String = s"hello, $name"
 *    }
 *    assert(greeting() == anonfun0())
 * }
 *  }}}
 */
trait Function0[@specialized(Specializable.Primitives) +R] extends AnyRef { self =>
  /** Apply the body of this function to the arguments.
   *  @return   the result of function application.
   */
  def apply(): R

  override def toString(): String = "<function0>"
}
