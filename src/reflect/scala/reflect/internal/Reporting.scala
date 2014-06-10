/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL, Typesafe Inc.
 * @author  Adriaan Moors
 */

package scala
package reflect
package internal

trait Reporting { self : Positions =>
  def inform(msg: String): Unit      = inform(NoPosition, msg)
  def warning(msg: String): Unit     = warning(NoPosition, msg)
  // globalError(msg: String) used to abort -- not sure that was a good idea, so I made it more regular
  // (couldn't find any uses that relied on old behavior)
  def globalError(msg: String): Unit = globalError(NoPosition, msg)

  def abort(msg: String): Nothing = {
    val augmented = supplementErrorMessage(msg)
    // Needs to call error to make sure the compile fails.
    globalError(augmented)
    throw new FatalError(augmented)
  }

  def inform(pos: Position, msg: String)      = Console.out.println(msg)
  def warning(pos: Position, msg: String)     = Console.err.println(msg)
  def globalError(pos: Position, msg: String) = Console.err.println(msg)

  def deprecationWarning(pos: Position, msg: String): Unit = warning(msg)

  /** Overridden when we know more about what was happening during a failure. */
  def supplementErrorMessage(msg: String): String = msg
}