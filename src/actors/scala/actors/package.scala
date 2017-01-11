package scala

/**
 * A library that provides both asynchronous and synchronous messaging to allow
 * for concurrent programming without explicit synchronization.
 *
 * == Guide ==
 *
 * A detailed guide for the actors library is available
 * [[http://docs.scala-lang.org/overviews/core/actors.html]].
 *
 * == Getting Started ==
 *
 * A starting point for using the actors library would be [[scala.actors.Reactor]],
 * [[scala.actors.ReplyReactor]], or [[scala.actors.Actor]] or their companion objects.
 *
 * @note As of release 2.10.1, replaced by <code>akka.actor</code> package. For migration of existing actors refer to the Actors Migration Guide.
 */
package object actors {

  // type of Reactors tracked by termination detector
  private[actors] type TrackedReactor = Reactor[A] forSome { type A >: Null }
}
