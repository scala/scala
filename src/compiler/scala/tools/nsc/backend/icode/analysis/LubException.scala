/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend.icode.analysis

class LubException(a: Any, b: Any, msg: String) extends Exception {
  override def toString() = "Lub error: " + msg + a + b
}
