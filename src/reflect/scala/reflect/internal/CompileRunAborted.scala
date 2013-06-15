/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */
package scala
package reflect.internal

/** A slightly more principled way of propagating information about
 *  compiler crashes. When abort is called, the message is wrapped up with
 *  a stack trace to be propagated upward. A compile run is wrapped at
 *  the only entry point to "Run", where this can be caught, unwrapped,
 *  and reported (or not) as appropriate.
 */
final case class CompileRunAborted(msg: String, caught: Throwable) extends Exception(msg, caught) { }
