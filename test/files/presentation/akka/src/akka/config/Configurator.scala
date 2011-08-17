/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.config

import akka.config.Supervision.{ SuperviseTypedActor, FaultHandlingStrategy }

private[akka] trait TypedActorConfiguratorBase {
  def getExternalDependency[T](clazz: Class[T]): T

  def configure(restartStrategy: FaultHandlingStrategy, components: List[SuperviseTypedActor]): TypedActorConfiguratorBase

  def inject: TypedActorConfiguratorBase

  def supervise: TypedActorConfiguratorBase

  def reset

  def stop
}
