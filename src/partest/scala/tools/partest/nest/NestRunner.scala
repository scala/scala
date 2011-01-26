/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

object NestRunner {
  def main(args: Array[String]) {
    new ReflectiveRunner main (args mkString " ")
  }
}
