/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

object Runner {
  def main(args: Array[String]): Unit = new ILoop process args
}
