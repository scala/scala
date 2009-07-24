/* NEST (New Scala Test)
 * Copyright 2007-2009 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

object NestRunner {
  def main(args: Array[String]) {
    val argstr = args.mkString(" ")
    (new ReflectiveRunner).main(argstr)
  }
}
