/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.runtime


import scala.runtime.ClassValueCompat._

private[scala] abstract class ClassValueCompat[T] extends ClassValueInterface[T] { self =>
  private val instance: ClassValueInterface[T] =
    if (classValueAvailable) new JavaClassValue()
    else new FallbackClassValue()

  private class JavaClassValue extends ClassValue[T] with ClassValueInterface[T] {
    override def computeValue(cls: Class[_]): T = self.computeValue(cls)
  }

  private class FallbackClassValue extends ClassValueInterface[T] {
    override def get(cls: Class[_]): T = self.computeValue(cls)

    override def remove(cls: Class[_]): Unit = {}
  }

  def get(cls: Class[_]): T = instance.get(cls)

  def remove(cls: Class[_]): Unit = instance.remove(cls)

  protected def computeValue(cls: Class[_]): T
}

private[scala] object ClassValueCompat {
  trait ClassValueInterface[T] {
    def get(cls: Class[_]): T

    def remove(cls: Class[_]): Unit
  }

  private val classValueAvailable: Boolean = try {
    Class.forName("java.lang.ClassValue", false, classOf[Object].getClassLoader)
    true
  } catch {
    case _: ClassNotFoundException => false
  }
}
