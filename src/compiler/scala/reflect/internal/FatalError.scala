/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.reflect.internal
case class FatalError(msg: String) extends Throwable(msg)
