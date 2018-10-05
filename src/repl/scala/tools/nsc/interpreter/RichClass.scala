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

package scala.tools.nsc
package interpreter

import scala.reflect.ClassTag

class RichClass[T](val clazz: Class[T]) {
  def toTag: ClassTag[T] = ClassTag[T](clazz)

  // Sadly isAnonymousClass does not return true for scala anonymous
  // classes because our naming scheme is not doing well against the
  // jvm's many assumptions.
  def isScalaAnonymous = (
    try clazz.isAnonymousClass || (clazz.getName contains "$anon$")
    catch { case _: java.lang.InternalError => false }  // good ol' "Malformed class name"
  )

  def supertags: List[ClassTag[_]] = supers map (_.toTag)
  def superNames: List[String]    = supers map (_.getName)
  def interfaces: List[JClass]    = supers filter (_.isInterface)

  def hasAncestorName(f: String => Boolean) = superNames exists f
  def hasAncestor(f: JClass => Boolean) = supers exists f

  def supers: List[JClass] = {
    def loop(x: JClass): List[JClass] = x.getSuperclass match {
      case null   => List(x)
      case sc     => x :: (x.getInterfaces.toList flatMap loop) ++ loop(sc)
    }
    loop(clazz).distinct
  }
}
