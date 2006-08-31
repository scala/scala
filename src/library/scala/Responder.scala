/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala

/** This object contains utility methods to build responders.
 *
 *  @author Burak Emir
 *  @version 1.0
 *
 *  @see class Responder
 *  @since revision 6897 (will be 2.1.1)
 */
object Responder {

  /** Creates a responder that answer continuations with the constant
   *  <code>a</code>.
   *
   *  @param x ...
   *  @return ...
   */
  def constant[a](x: a) = new Responder[a] {
    def respond(k: a => unit) = k(x)
  }

  /** Executes <code>x</code> and returns <code>true</code>, useful
   *  as syntactic convenience in for comprehensions.
   *
   *  @param x ...
   *  @return ...
   */
  def exec[a](x: => unit): boolean = { x; true  }

  /** runs a responder, returning an optional result
  */
  def run[a](r: Responder[a]): Option[a] = {
    var result: Option[a] = None
    r.foreach(x => result = Some(x))
    result
  }

  def loop[a](r: Responder[unit]): Responder[Nothing] =
    for (val _ <- r; val y <- loop(r)) yield y

  def loopWhile[a](cond: => boolean)(r: Responder[unit]): Responder[unit] =
    if (cond) for (val _ <- r; val y <- loopWhile(cond)(r)) yield y
    else constant(())

}

/** Instances of responder are the building blocks of small programs
 *  written in continuation passing style. By using responder classes
 *  in for comprehensions, one can embed domain-specific languages in
 *  Scala while giving the impression that programs in these DSLs are
 *  written in direct style.
 *
 *  @author Burak Emir
 *  @version 1.0
 *
 *  @since revision 6897 (will be 2.1.1)
 */
abstract class Responder[+a] {

  def respond(k: a => unit): unit

  def foreach(k: a => unit): unit = respond(k)

  def map[b](f: a => b) = new Responder[b] {
    def respond(k: b => unit): unit =
      Responder.this.respond(x => k(f(x)))
  }

  def flatMap[b](f: a => Responder[b]) = new Responder[b] {
    def respond(k: b => unit): unit =
      Responder.this.respond(x => f(x).respond(k))
  }

  def filter(p: a => boolean) = new Responder[a] {
    def respond(k: a => unit): unit =
      Responder.this.respond(x => if (p(x)) k(x) else ())
  }
}

