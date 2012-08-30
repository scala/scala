/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools

package object nsc {
  type Phase = scala.reflect.internal.Phase
  val NoPhase = scala.reflect.internal.NoPhase

  type FatalError = scala.reflect.internal.FatalError
  val FatalError = scala.reflect.internal.FatalError

  type MissingRequirementError = scala.reflect.internal.MissingRequirementError
  val MissingRequirementError = scala.reflect.internal.MissingRequirementError

  val ListOfNil = List(Nil)

  // Creates a string interpolator called 'sm' which
  // acts like 's' but calls stripMargin before returning
  // the string.
  implicit class StripMarginOps(val sc: StringContext) {
    def sm(xs: Any*): String = sc.standardInterpolator(s => StringContext.treatEscapes(s).stripMargin, xs)
  }
}
