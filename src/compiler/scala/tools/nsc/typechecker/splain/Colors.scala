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

package scala.tools.nsc
package typechecker
package splain

trait StringColor
{
  def color(s: String, col: String): String
}

object StringColors
{
  implicit val noColor =
    new StringColor {
      def color(s: String, col: String) = s
    }

  implicit val color =
    new StringColor {
      import Console.RESET

      def color(s: String, col: String) = col + s + RESET
    }
}

object StringColor
{
  implicit class StringColorOps(s: String)(implicit sc: StringColor)
  {
    import Console._
    def red = sc.color(s, RED)
    def green = sc.color(s, GREEN)
    def yellow = sc.color(s, YELLOW)
    def blue = sc.color(s, BLUE)
  }
}
