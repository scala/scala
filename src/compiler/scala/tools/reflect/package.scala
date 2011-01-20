/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools

import java.lang.reflect.Method
import java.{ lang => jl }

package object reflect {
  def nameAndArity(m: Method) = (m.getName, m.getParameterTypes.size)
  def allInterfaces(cl: Class[_]): List[Class[_]] =
    if (cl == null) Nil
    else cl.getInterfaces.toList ++ allInterfaces(cl.getSuperclass) distinct

  def methodsNamed(target: AnyRef, name: String): List[Method] =
    target.getClass.getMethods filter (x => x.getName == name) toList

  /** If there is a single non-bridge apply method in the given instance,
   *  return it: otherwise None.
   */
  def uniqueApply(target: AnyRef) = {
    methodsNamed(target, "apply") filterNot (_.isBridge) match {
      case List(x)  => Some(x)
      case _        => None
    }
  }

  def zeroOfClass(clazz: Class[_]) = zeroOf(Manifest.classType(clazz))
  def zeroOf[T](implicit m: Manifest[T]): AnyRef = {
    if (m == manifest[Boolean] || m == manifest[jl.Boolean]) false: jl.Boolean
    else if (m == manifest[Unit] || m == manifest[jl.Void] || m == manifest[scala.runtime.BoxedUnit]) scala.runtime.BoxedUnit.UNIT
    else if (m == manifest[Char] || m == manifest[jl.Character]) 0.toChar: jl.Character
    else if (m == manifest[Byte] || m == manifest[jl.Byte]) 0.toByte: jl.Byte
    else if (m == manifest[Short] || m == manifest[jl.Short]) 0.toShort: jl.Short
    else if (m == manifest[Int] || m == manifest[jl.Integer]) 0: jl.Integer
    else if (m == manifest[Long] || m == manifest[jl.Long]) 0l: jl.Long
    else if (m == manifest[Float] || m == manifest[jl.Float]) 0f: jl.Float
    else if (m == manifest[Double] || m == manifest[jl.Double]) 0d: jl.Double
    else null
  }
}
