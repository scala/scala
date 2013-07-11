/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala
package reflect.internal
case class FatalError(msg: String) extends Exception(msg)
