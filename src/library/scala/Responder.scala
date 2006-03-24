package scala

/** contains utility methods to build responders
 *  @see class Responder
 *  @since revision 6897 (will be 2.1.1)
 */
object Responder {

  /** creates a responder that answer continuations with the constant a */
  def constant[a](x: a) = new Responder[a] {
    def answer(k: a => unit) = k(x)
  }

  /** executes x and returns true, useful as syntactic convenience in
  	* for comprehensions
	*/
  def exec[a](x: => unit): boolean = { x; true  }

  /** runs a responder, returning an optional result
  */
  def run[a](r: Responder[a]): Option[a] = {
    var result: Option[a] = None
    r.answer(x => result = Some(x))
    result
  }
}

/** instances of responder are the building blocks of small programs
 *  written in continuation passing style. By using responder classes
 *  in for comprehensions, one can embed domain-specific languages in
 *  Scala while giving the impression that programs in these DSLs are
 *  written in direct style.
 *  @since revision 6897 (will be 2.1.1)
 */
abstract class Responder[a] {

  def answer(k: a => unit): unit

  def map[b](f: a => b) = new Responder[b] {
    def answer(k: b => unit): unit =
      Responder.this.answer(x => k(f(x)))
  }

  def flatMap[b](f: a => Responder[b]) = new Responder[b] {
    def answer(k: b => unit): unit =
      Responder.this.answer(x => f(x).answer(k))
  }

  def filter(p: a => boolean) = new Responder[a] {
    def answer(k: a => unit): unit =
      Responder.this.answer(x => if (p(x)) k(x) else ())
  }
}
