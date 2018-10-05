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
// genprod generated these sources at: Fri Oct 05 11:04:52 CEST 2018

package scala


/** A function of 0 parameters.
 *  
 *  In the following example, the definition of javaVersion is a
 *  shorthand for the anonymous class definition anonfun0:
 *
 *  {{{
 *  object Main extends App {
 *    val javaVersion = () => sys.props("java.version")
 *
 *    val anonfun0 = new Function0[String] {
 *      def apply(): String = sys.props("java.version")
 *    }
 *    assert(javaVersion() == anonfun0())
 * }
 *  }}}
 */
trait Function0[@specialized(Specializable.Primitives) +R] extends AnyRef { self =>
  /** Apply the body of this function to the arguments.
   *  @return   the result of function application.
   */
  def apply(): R

  override def toString() = "<function0>"
}
