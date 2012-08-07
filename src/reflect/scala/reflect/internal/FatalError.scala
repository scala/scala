/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.reflect.internal
case class FatalError(msg: String) extends Exception(msg)
