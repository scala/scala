/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors
package migration

import scala.actors.{ Actor, ActorRef, InternalActorRef }
import scala.collection.immutable
import scala.reflect.ClassTag

object ActorDSL {

  private[migration] val contextStack = new ThreadLocal[immutable.Stack[Boolean]] {
    override def initialValue() = immutable.Stack[Boolean]()
  }

  private[this] def withCleanContext(block: => ActorRef): ActorRef = {
    // push clean marker
    val old = contextStack.get
    contextStack.set(old.push(true))
    try {
      val instance = block

      if (instance eq null)
        throw new Exception("ActorRef can't be 'null'")

      instance
    } finally {
      val stackAfter = contextStack.get
      if (stackAfter.nonEmpty)
        contextStack.set(if (!stackAfter.head) stackAfter.pop.pop else stackAfter.pop)
    }
  }
  
  /**
   * Create an actor from the given thunk which must produce an [[scala.actors.Actor]].
   *
   * @param ctor is a by-name argument which captures an [[scala.actors.Actor]]
   *        factory; <b>do not make the generated object accessible to code
   *        outside and do not return the same object upon subsequent invocations.</b>
   */
  def actor[T <: InternalActor: ClassTag](ctor: ⇒ T): ActorRef = {
    withCleanContext {
      val newActor = ctor
      val newRef = new InternalActorRef(newActor)
      newActor.start()
      newRef
    }
  }

}
