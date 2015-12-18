/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */
package scala
package reflect
package internal

import scala.language.implicitConversions

import java.lang.annotation.{ Annotation => jAnnotation }
import java.lang.reflect.{
  Member => jMember, Constructor => jConstructor, Method => jMethod,
  AnnotatedElement => jAnnotatedElement, Type => jType,
  TypeVariable => jTypeVariable
}

/** This class tries to abstract over some of the duplication
 *  in java.lang.reflect.{ Method, Constructor }.
 */
class JMethodOrConstructor(val member: jMember with jAnnotatedElement) {
  def isVarArgs: Boolean = member match {
    case m: jMethod         => m.isVarArgs
    case m: jConstructor[_] => m.isVarArgs
  }
  def typeParams: Array[_ <: jTypeVariable[_]] = member match {
    case m: jMethod         => m.getTypeParameters
    case m: jConstructor[_] => m.getTypeParameters
  }
  def paramTypes: Array[jType] = member match {
    case m: jMethod         => m.getGenericParameterTypes
    case m: jConstructor[_] => m.getGenericParameterTypes
  }
  def paramAnnotations: Array[Array[jAnnotation]] = member match {
    case m: jMethod         => m.getParameterAnnotations
    case m: jConstructor[_] => m.getParameterAnnotations
  }
  def resultType: jType = member match {
    case m: jMethod         => m.getGenericReturnType
    case m: jConstructor[_] => classOf[Unit]
  }
}

object JMethodOrConstructor {
  implicit def liftMethodToJmoc(m: jMethod): JMethodOrConstructor              = new JMethodOrConstructor(m)
  implicit def liftConstructorToJmoc(m: jConstructor[_]): JMethodOrConstructor = new JMethodOrConstructor(m)
}
