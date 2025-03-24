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

package scala.tools.testkit

import scala.reflect.{ ClassTag, classTag, ensureAccessible }
import scala.util.chaining._
import java.lang.reflect.{ Array => _, _ }

/** This module contains reflection-related utilities.
 *
 *  This object contains methods that will not work on Scala.js nor Scala
 *  Native, making any test using `ReflectUtil` JVM-only.
 */
object ReflectUtil {
  private lazy val modsField = ensureAccessible {
    try ensureAccessible(classOf[Class[_]].getDeclaredMethod("getDeclaredFields0", classOf[Boolean]))
      .invoke(classOf[Field], false).asInstanceOf[Array[Field]]
      .findLast(_.getName == "modifiers")
      .getOrElse(getModsField)
    catch { case _: NoSuchMethodException => getModsField }
  }

  private def getModsField = classOf[Field].getDeclaredField("modifiers")

  def getFieldAccessible[T: ClassTag](n: String): Field =
    classTag[T]
      .runtimeClass.getDeclaredField(n)
      .tap { f =>
        if ((f.getModifiers & Modifier.FINAL) != 0)
          modsField.setInt(f, f.getModifiers() & ~Modifier.FINAL)
        if ((f.getModifiers & Modifier.PUBLIC) == 0)
          f.setAccessible(true)
      }

  def getFinalFieldAccessible[T: ClassTag](n: String): Field =
    classTag[T]
      .runtimeClass.getDeclaredField(n)
      .tap { f =>
        if ((f.getModifiers & Modifier.PUBLIC) == 0)
          f.setAccessible(true)
      }

  // finds method with exact name or name$suffix but not name$default$suffix
  def getMethodAccessible[A: ClassTag](name: String): Method =
    implicitly[ClassTag[A]]
      .runtimeClass.getDeclaredMethods
      .find(nameMatches(_, name)) match {
        case Some(m) => m.tap(_.setAccessible(true))
        case None    => AssertUtil.fail(s"Missing method $name")
      }

  private def nameMatches(m: Method, name: String): Boolean =
    m.getName.startsWith(name) &&
    (m.getName.length == name.length ||
     m.getName.charAt(name.length) == '$' && !m.getName.substring(name.length).startsWith("$default$"))

  implicit class MethodOps(val m: Method) extends AnyVal {
    def invokeAs[A](receiver: AnyRef, args: AnyRef*): A =
      try m.invoke(receiver, args: _*).asInstanceOf[A]
      catch {
        case e: IllegalArgumentException if e.getMessage == "wrong number of arguments" =>
          def required =
            m.getParameterCount match {
              case 0 => "0"
              case n => s"${m.getParameterCount}: (${m.getGenericParameterTypes.mkString(", ")})"
            }
          throw new IllegalArgumentException(s"wrong number of arguments: ${args.length}; required: $required")
      }
  }
}
