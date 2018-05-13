/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala
package tools

package object cmd {
  // make some language features in this package compile without warning
  implicit def implicitConversions = scala.language.implicitConversions
  implicit def postfixOps = scala.language.postfixOps

  def toArgs(line: String): List[String]   = CommandLineParser tokenize line
  def fromArgs(args: List[String]): String = args mkString " "
}
