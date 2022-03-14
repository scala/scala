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
import scala.annotation.nowarn

private[runtime] object ModuleSerializationProxy {
  private val instances = new ClassValueCompat[Object] {
    @nowarn("cat=deprecation") // AccessController is deprecated on JDK 17
    def getModule(cls: Class[_]): Object =
      java.security.AccessController.doPrivileged(
        (() => cls.getField("MODULE$").get(null)): PrivilegedExceptionAction[Object])
    override protected def computeValue(cls: Class[_]): Object =
      try getModule(cls)
      catch {
        case e: PrivilegedActionException =>
          rethrowRuntime(e.getCause)
      }
  }

  private def rethrowRuntime(e: Throwable): Object = e match {
    case re: RuntimeException => throw re
    case _ => throw new RuntimeException(e)
  }
}

@SerialVersionUID(1L)
final class ModuleSerializationProxy(moduleClass: Class[_]) extends Serializable {
  private def readResolve = ModuleSerializationProxy.instances.get(moduleClass)
}
