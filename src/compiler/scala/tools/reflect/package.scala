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

  def zeroOfClass(clazz: Class[_]) = zeroOf(ClassTag(clazz))
  def zeroOf[T](implicit t: ClassTag[T]): AnyRef = {
    if (t == classTag[Boolean] || t == classTag[jl.Boolean]) false: jl.Boolean
    else if (t == classTag[Unit] || t == classTag[jl.Void] || t == classTag[scala.runtime.BoxedUnit]) scala.runtime.BoxedUnit.UNIT
    else if (t == classTag[Char] || t == classTag[jl.Character]) 0.toChar: jl.Character
    else if (t == classTag[Byte] || t == classTag[jl.Byte]) 0.toByte: jl.Byte
    else if (t == classTag[Short] || t == classTag[jl.Short]) 0.toShort: jl.Short
    else if (t == classTag[Int] || t == classTag[jl.Integer]) 0: jl.Integer
    else if (t == classTag[Long] || t == classTag[jl.Long]) 0l: jl.Long
    else if (t == classTag[Float] || t == classTag[jl.Float]) 0f: jl.Float
    else if (t == classTag[Double] || t == classTag[jl.Double]) 0d: jl.Double
    else null
  }
}
