/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors

/** An enclosing trait for reactions.
 *  Examples of reactions are: actor.receive's, event.handle's, etc
 *
 *  @author Martin Odersky
 *  @version 1.0
 *
 *  @param m The input type of a Reaction; typically the type of messages or events.
 */
trait Reactions[m] {

  /** The partial function underlying a reaction. Note that this is formulated
   *  in CPS style.
   */
  type Re[r] = PartialFunction[m, (r => unit) => unit]

  /** Activate the given partial function `f', for instance by reading
   *  a message or waiting for an event, and applying `f' to the result.
   */
  def activate[r](f: Re[r]): (r => unit) => unit

  /** The class of reactions
   *  @param r     The type of values returned by the reaction.
   *               (More precisely, the type of values passed to its continuation)
   *  @param fun   The partial function underlying a reaction
   */
  class Reaction[+r](private val fun: Re[r]) extends Responder[r] {

    def respond(k: r => unit): unit = activate(fun)(k)

    override def map[s](f: r => s) = new Reaction[s] (
      fun andThen {
        result: ((r => unit) => unit) =>
          k: (s => unit) => result((x: r) => k(f(x)))
      }
    )

    def flatMap[s](f: r => Reaction[s]) = new Reaction[s] (
      fun andThen {
        result: ((r => unit) => unit) =>
          k: (s => unit) => result((x: r) => f(x).respond(k))
      }
    )

    override def filter(p: r => boolean) = new Reaction[r] (
      fun andThen {
        result: ((r => unit) => unit) =>
          k: (r => unit) => result((x: r) => if (p(x)) k(x) else ())
      }
    )

    def orElse[r1 >: r](that: Reaction[r1]) = new Reaction[r1] (
      this.fun orElse that.fun
    )
  }
}
