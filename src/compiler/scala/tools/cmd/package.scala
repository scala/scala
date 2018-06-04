/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala
package tools

package object cmd {
  def toArgs(line: String): List[String]   = CommandLineParser tokenize line
  def fromArgs(args: List[String]): String = args mkString " "
}
