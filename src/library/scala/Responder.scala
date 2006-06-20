package scala

/** contains utility methods to build responders
 *  @see class Responder
 *  @since revision 6897 (will be 2.1.1)
 */
object Responder {

  /** creates a responder that answer continuations with the constant a */
  def constant[a](x: a) = new Responder[a] {
    def foreach(k: a => unit) = k(x)
  }

  /** executes x and returns true, useful as syntactic convenience in
   * for comprehensions
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
 *  @since revision 6897 (will be 2.1.1)
 */
abstract class Responder[+a] {

  def foreach(k: a => unit): unit

  def map[b](f: a => b) = new Responder[b] {
    def foreach(k: b => unit): unit =
      Responder.this.foreach(x => k(f(x)))
  }

  def flatMap[b](f: a => Responder[b]) = new Responder[b] {
    def foreach(k: b => unit): unit =
      Responder.this.foreach(x => f(x).foreach(k))
  }

  def filter(p: a => boolean) = new Responder[a] {
    def foreach(k: a => unit): unit =
      Responder.this.foreach(x => if (p(x)) k(x) else ())
  }

}

