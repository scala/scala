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

package scala.tools.nsc.tasty

import scala.language.implicitConversions

import ForceKinds._

object ForceKinds {

  /** When forcing the companion of a module */
  final val DeepForce: ForceKinds.Single = of(1 << 1)
  /** When forcing the owner of a symbol */
  final val CompleteOwner: ForceKinds.Single = of(1 << 2)
  /** When forcing an overloaded signature */
  final val OverloadedSym: ForceKinds.Single = of(1 << 3)
  /** When forcing a symbol that will be copied */
  final val CopySym: ForceKinds.Single = of(1 << 4)
  /** When forcing the underlying symbol of some type space */
  final val SpaceForce: ForceKinds.Single = of(1 << 5)
  /** When forcing the enum singleton from its "fake" module class */
  final val EnumProxy: ForceKinds.Single = of(1 << 6)

  private def of(mask: Int): ForceKinds.Single = new ForceKinds.Single(mask)

  class Single(val toInt: Int) extends AnyVal { mode =>
    def |(single: ForceKinds.Single): ForceKinds = new ForceKinds(toInt | single.toInt)
  }

  @inline implicit def single2ForceKinds(single: ForceKinds.Single): ForceKinds = new ForceKinds(single.toInt)

}

/**A static type representing a bitset of modes that are for debugging why a symbol may have been forced
 */
class ForceKinds(val toInt: Int) extends AnyVal {
  def is(single: ForceKinds.Single): Boolean = (toInt & single.toInt) == single.toInt
  def |(single: ForceKinds.Single): ForceKinds = new ForceKinds(toInt | single.toInt)

  def describe: List[String] = {
    var xs = List.empty[String]
    if (is(DeepForce)) xs ::= "deep"
    if (is(CompleteOwner)) xs ::= "class owner is required"
    if (is(OverloadedSym)) xs ::= "overload resolution"
    if (is(CopySym)) xs ::= "copying its info"
    if (is(SpaceForce)) xs ::= "space"
    if (is(EnumProxy)) xs ::= "forcing enum value from fake object"
    xs
  }
}
