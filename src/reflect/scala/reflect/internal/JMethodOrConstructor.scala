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

package scala
package reflect
package internal

import scala.language.implicitConversions

import java.lang.annotation.{ Annotation => jAnnotation }
import java.lang.reflect.{
  Constructor => jConstructor, Method => jMethod,
  Type => jType, TypeVariable => jTypeVariable
}

/** This class tries to abstract over some of the duplication
 *  in java.lang.reflect.{ Method, Constructor }.
 */
sealed abstract class JMethodOrConstructor {
  def isVarArgs: Boolean = this match {
    case JMethod(m)      => m.isVarArgs
    case JConstructor(m) => m.isVarArgs
  }
  def typeParams: Array[_ <: jTypeVariable[_]] = this match {
    case JMethod(m)      => m.getTypeParameters
    case JConstructor(m) => m.getTypeParameters
  }
  def paramTypes: Array[jType] = this match {
    case JMethod(m)      => m.getGenericParameterTypes
    case JConstructor(m) => m.getGenericParameterTypes
  }
  def paramAnnotations: Array[Array[jAnnotation]] = this match {
    case JMethod(m)      => m.getParameterAnnotations
    case JConstructor(m) => m.getParameterAnnotations
  }
  def resultType: jType = this match {
    case JMethod(m)      => m.getGenericReturnType
    case JConstructor(_) => classOf[Unit]
  }
}

object JMethodOrConstructor {
  implicit def liftMethodToJmoc(m: jMethod): JMethodOrConstructor              = JMethod(m)
  implicit def liftConstructorToJmoc(m: jConstructor[_]): JMethodOrConstructor = JConstructor(m)
}

final case class JMethod(m: jMethod)              extends JMethodOrConstructor
final case class JConstructor(m: jConstructor[_]) extends JMethodOrConstructor
