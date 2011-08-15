package scala

/**
 * A library that provides both asynchronous and synchronous messaging to allow
 * for concurrent programming without explicit synchronization.
 *
 * == Guide ==
 *
 * A detailed guide for the actors library is available
 * [[http://www.scala-lang.org/docu/files/actors-api/actors_api_guide.html#]].
 *
 * == Getting Started ==
 *
 * A starting point for using the actors library would be [[scala.actors.Reactor]],
 * [[scala.actors.ReplyReactor]], or [[scala.actors.Actor]] or their companion objects.
 *
 */
package object actors {

  // type of Reactors tracked by termination detector
  private[actors] type TrackedReactor = Reactor[A] forSome { type A >: Null }
}
