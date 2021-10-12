/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.runtime

import java.io.Serializable
import java.security.PrivilegedActionException
import java.security.PrivilegedExceptionAction

private[runtime] object ModuleSerializationProxy {
  private val instances = new ClassValueCompat[Object] {
    override protected def computeValue(cls: Class[_]): Object = {
      try {
        java.security.AccessController.doPrivileged((() => cls.getField("MODULE$").get(null)): PrivilegedExceptionAction[Object])
      } catch {
        case e: PrivilegedActionException =>
          rethrowRuntime(e.getCause)
      }
    }
  }

  private def rethrowRuntime(e: Throwable): Object = {
    val cause = e.getCause
    cause match {
      case exception: RuntimeException => throw exception
      case _ => throw new RuntimeException(cause)
    }
  }
}

@SerialVersionUID(1L)
final class ModuleSerializationProxy(moduleClass: Class[_]) extends Serializable {
  private def readResolve = ModuleSerializationProxy.instances.get(moduleClass)
}
