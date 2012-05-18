package scala.actors

import scala.collection._

object MigrationSystem {

  private[actors] val contextStack = new ThreadLocal[immutable.Stack[Boolean]] {
    override def initialValue() = immutable.Stack[Boolean]()
  }

  private[this] def withCleanContext(block: => ActorRef): ActorRef = {
    // push clean marker
    val old = contextStack.get
    contextStack.set(old.push(true))
    try {
      val instance = block

      if (instance eq null)
        throw new Exception("Actor instance passed to actorOf can't be 'null'")

      instance
    } finally {
      val stackAfter = contextStack.get
      if (stackAfter.nonEmpty)
        contextStack.set(if (!stackAfter.head) stackAfter.pop.pop else stackAfter.pop)
    }
  }

  def actorOf(props: Props): ActorRef = withCleanContext {
    val creator = props.creator()
    val r = new InternalActorRef(creator)
    creator.start()
    r
  }

}