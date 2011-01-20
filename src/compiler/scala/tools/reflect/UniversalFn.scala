/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools
package reflect

import java.lang.reflect.{ Method, InvocationTargetException }
import java.{ lang => jl }

/** For certain reflection tasks it is convenient to treat all methods
 *  as having the same signature: (Seq[AnyRef]) => AnyRef
 *
 *  That is the "universal signature" and UniversalFn exists to provide
 *  it without abandoning the information we had before we needed it.
 *  One place this is used: closures can pose as arbitrary interfaces,
 *  and this is how we route the arguments from the actual method
 *  invocation (which targets a proxy object) to the original closure.
 */
class UniversalFn private (val closure: AnyRef, val method: Method) extends (Seq[AnyRef] => AnyRef) {
  universal =>

  /** Given an interface type argument, creates a proxy object of the
   *  type of the interface which implements all its methods by routing
   *  them to this universal function.  Will throw an exception in the
   *  face of any bad data.
   */
  def as[T: Manifest] : T = {
    val clazz = manifest[T].erasure
    require(clazz.isInterface, "Type argument must be an interface.")

    val interfaceMethods = clazz.getDeclaredMethods.toSet
    val proxy = Mock.fromInterfaces(clazz) {
      case Invoked(_, m, args) if interfaceMethods(m) => universal(args)
    }
    proxy.asInstanceOf[T]
  }

  def apply(xs: Seq[AnyRef]): AnyRef =
    try method.invoke(closure, xs: _*)
    catch { case x: InvocationTargetException => throw x.getCause() }
}

object UniversalFn {
  /** We use a private constructor so we can enforce some rules: we don't want
   *  universal functions to stack up, and right now we will only allow objects
   *  which appear to be closures (there's no reason not to eventually lift
   *  this restriction, but it should be harder to shoot your foot first.)
   */
  def apply(closure: AnyRef): UniversalFn = closure match {
    case x: UniversalFn => x
    case _              =>
      val m = uniqueApply(closure) getOrElse {
        throw new IllegalArgumentException("Argument must have exactly one non-bridge apply method.")
      }
      new UniversalFn(closure, m)
  }
}
