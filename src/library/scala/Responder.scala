/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala

/** This object contains utility methods to build responders.
 *
 *  @author Martin Odersky
 *  @author Burak Emir
 *  @version 1.0
 *
 *  @see class Responder
 *  @since 2.1
 */
@deprecated("this object will be removed", "2.11.0")
object Responder {

  /** Creates a responder that answer continuations with the constant `a`.
   */
  def constant[A](x: A) = new Responder[A] {
    def respond(k: A => Unit) = k(x)
  }

  /** Executes `x` and returns `'''true'''`, useful as syntactic
   *  convenience in for comprehensions.
   */
  def exec[A](x: => Unit): Boolean = { x; true }

  /** Runs a responder, returning an optional result.
  */
  def run[A](r: Responder[A]): Option[A] = {
    var result: Option[A] = None
    r.foreach(x => result = Some(x))
    result
  }

  def loop[A](r: Responder[Unit]): Responder[Nothing] =
    for (_ <- r; y <- loop(r)) yield y

  def loopWhile[A](cond: => Boolean)(r: Responder[Unit]): Responder[Unit] =
    if (cond) for (_ <- r; y <- loopWhile(cond)(r)) yield y
    else constant(())
}

/** Instances of responder are the building blocks of small programs
 *  written in continuation passing style. By using responder classes
 *  in for comprehensions, one can embed domain-specific languages in
 *  Scala while giving the impression that programs in these DSLs are
 *  written in direct style.
 *
 *  @author Martin Odersky
 *  @author Burak Emir
 *  @version 1.0
 *  @since 2.1
 */
@deprecated("this class will be removed", "2.11.0")
abstract class Responder[+A] extends Serializable {

  def respond(k: A => Unit): Unit

  def foreach(k: A => Unit) { respond(k) }

  def map[B](f: A => B) = new Responder[B] {
    def respond(k: B => Unit) {
      Responder.this.respond(x => k(f(x)))
    }
  }

  def flatMap[B](f: A => Responder[B]) = new Responder[B] {
    def respond(k: B => Unit) {
      Responder.this.respond(x => f(x).respond(k))
    }
  }

  def filter(p: A => Boolean) = new Responder[A] {
    def respond(k: A => Unit) {
      Responder.this.respond(x => if (p(x)) k(x) else ())
    }
  }

  override def toString = "Responder"
}
