/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package reflect

import java.lang.reflect.{ Method, Proxy }

/** A class representing a single method call.  It is primarily for use
 *  in tandem with Mock.  If the invocation did not target an InvocationHandler,
 *  proxy will be null.
 */
class Invoked private (val proxy: AnyRef, val m: Method, val args: List[AnyRef]) {
  def name                 = m.getName
  def arity                = m.getParameterTypes.size
  def returnType           = m.getReturnType
  def returns[T: Manifest] = returnType == manifest[T].erasure

  def invokeOn(target: AnyRef) = m.invoke(target, args: _*)
  def isObjectMethod = Set("toString", "equals", "hashCode") contains name

  override def toString = "Invoked: %s called with %s".format(
    m.getName,
    if (args.isEmpty) "no args" else "args '%s'".format(args mkString ", ")
  )
}

object Invoked {
  def apply(m: Method, args: Seq[Any]): Invoked = apply(null, m, args)
  def apply(proxy: AnyRef, m: Method, args: Seq[Any]): Invoked = {
    val fixedArgs = if (args == null) Nil else args.toList map (_.asInstanceOf[AnyRef])
    new Invoked(proxy, m, fixedArgs)
  }
  def unapply(x: Any) = x match {
    case x: Invoked => Some(x.proxy, x.m, x.args)
    case _          => None
  }
  object NameAndArgs {
    def unapply(x: Any) = x match {
      case x: Invoked => Some(x.name, x.args)
      case _          => None
    }
  }
  object NameAndArity {
    def unapply(x: Any) = x match {
      case x: Invoked => Some(x.name, x.arity)
      case _          => None
    }
  }
}
