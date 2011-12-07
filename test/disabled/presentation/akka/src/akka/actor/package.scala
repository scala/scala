/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka

import actor.{ ScalaActorRef, ActorRef }

package object actor {
  implicit def actorRef2Scala(ref: ActorRef): ScalaActorRef =
    ref.asInstanceOf[ScalaActorRef]

  implicit def scala2ActorRef(ref: ScalaActorRef): ActorRef =
    ref.asInstanceOf[ActorRef]

  type Uuid = com.eaio.uuid.UUID

  def newUuid(): Uuid = new Uuid()

  def uuidFrom(time: Long, clockSeqAndNode: Long): Uuid = new Uuid(time, clockSeqAndNode)

  def uuidFrom(uuid: String): Uuid = new Uuid(uuid)
}
