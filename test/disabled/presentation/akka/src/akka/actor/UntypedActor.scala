/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.actor

import akka.japi.{ Creator, Procedure }

/**
 * Subclass this abstract class to create a MDB-style untyped actor.
 * <p/>
 * This class is meant to be used from Java.
 * <p/>
 * Here is an example on how to create and use an UntypedActor:
 * <pre>
 *  public class SampleUntypedActor extends UntypedActor {
 *    public void onReceive(Object message) throws Exception {
 *      if (message instanceof String) {
 *        String msg = (String)message;
 *
 *        if (msg.equals("UseReply")) {
 *          // Reply to original sender of message using the 'replyUnsafe' method
 *          getContext().replyUnsafe(msg + ":" + getContext().getUuid());
 *
 *        } else if (msg.equals("UseSender") && getContext().getSender().isDefined()) {
 *          // Reply to original sender of message using the sender reference
 *          // also passing along my own reference (the context)
 *          getContext().getSender().get().sendOneWay(msg, context);
 *
 *        } else if (msg.equals("UseSenderFuture") && getContext().getSenderFuture().isDefined()) {
 *          // Reply to original sender of message using the sender future reference
 *          getContext().getSenderFuture().get().completeWithResult(msg);
 *
 *        } else if (msg.equals("SendToSelf")) {
 *          // Send message to the actor itself recursively
 *          getContext().sendOneWay(msg)
 *
 *        } else if (msg.equals("ForwardMessage")) {
 *          // Retrieve an actor from the ActorRegistry by ID and get an ActorRef back
 *          ActorRef actorRef = Actor.registry.actorsFor("some-actor-id").head();
 *
 *        } else throw new IllegalArgumentException("Unknown message: " + message);
 *      } else throw new IllegalArgumentException("Unknown message: " + message);
 *    }
 *
 *    public static void main(String[] args) {
 *      ActorRef actor = Actors.actorOf(SampleUntypedActor.class);
 *      actor.start();
 *      actor.sendOneWay("SendToSelf");
 *      actor.stop();
 *    }
 *  }
 * </pre>
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
abstract class UntypedActor extends Actor {

  /**
   * To be implemented by concrete UntypedActor. Defines the message handler.
   */
  @throws(classOf[Exception])
  def onReceive(message: Any): Unit

  /**
   * Returns the 'self' reference with the API.
   */
  def getContext(): ActorRef = self

  /**
   * Returns the 'self' reference with the API.
   */
  def context(): ActorRef = self

  /**
   * Java API for become
   */
  def become(behavior: Procedure[Any]): Unit = become(behavior, false)

  /*
   * Java API for become with optional discardOld
   */
  def become(behavior: Procedure[Any], discardOld: Boolean): Unit =
    super.become({ case msg => behavior.apply(msg) }, discardOld)

  /**
   * User overridable callback.
   * <p/>
   * Is called when an Actor is started by invoking 'actor.start()'.
   */
  override def preStart() {}

  /**
   * User overridable callback.
   * <p/>
   * Is called when 'actor.stop()' is invoked.
   */
  override def postStop() {}

  /**
   * User overridable callback.
   * <p/>
   * Is called on a crashed Actor right BEFORE it is restarted to allow clean up of resources before Actor is terminated.
   */
  override def preRestart(reason: Throwable) {}

  /**
   * User overridable callback.
   * <p/>
   * Is called right AFTER restart on the newly created Actor to allow reinitialization after an Actor crash.
   */
  override def postRestart(reason: Throwable) {}

  /**
   * User overridable callback.
   * <p/>
   * Is called when a message isn't handled by the current behavior of the actor
   * by default it throws an UnhandledMessageException
   */
  override def unhandled(msg: Any) {
    throw new UnhandledMessageException(msg, self)
  }

  final protected def receive = {
    case msg => onReceive(msg)
  }
}

/**
 * Factory closure for an UntypedActor, to be used with 'Actors.actorOf(factory)'.
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
trait UntypedActorFactory extends Creator[Actor]
