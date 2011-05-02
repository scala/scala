/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package util

import scala.tools.nsc._

/** A tool for identifying which classfile is being used.
 *  under the given conditions.
 */
object Which {
  def main(args: Array[String]): Unit = {
    val settings = new Settings()
    val names = settings.processArguments(args.toList, true)._2
    val global = new Global(settings)
    val cp = global.classPath

    import cp._

    for (name <- names) {
      def fail() = println("Could not find: %s".format(name))
      (cp findClass name) match {
        case Some(classRep) => classRep.binary match {
          case Some(f)  => println("%s is %s".format(name, f))
          case _        => fail
        }
        case _ => fail
      }
    }
  }
}




