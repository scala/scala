/* NSC -- new Scala compiler
 * Copyright 2013-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect.internal

package object annotations {
  @deprecated("Use scala.annotation.compileTimeOnly instead", "2.11.0")
  type compileTimeOnly = scala.annotation.compileTimeOnly
}