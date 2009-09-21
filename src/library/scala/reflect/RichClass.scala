/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.reflect

import scala.annotation.experimental
import scala.util.control.Exception._
import scala.util.ScalaClassLoader._
import java.lang.{ Class => JClass }
import java.lang.reflect. { Constructor => JConstructor }

object RichClass
{
  // We can't put this in Predef at the moment because everything referenced
  // from Predef has to be buildable at the first bootstraping phase.
  implicit def classWrapper[T](x: JClass[T]): RichClass[T] = new RichClass(x)
}

@experimental
final class RichClass[T](val self: JClass[T]) extends Proxy
{
  // The getConstructors and getDeclaredConstructors methods on java.lang.Class[T]
  // return "raw type" Constructors rather than Constructor[T]s as one would want.
  // The "why" from http://java.sun.com/javase/6/docs/api/java/lang/Class.html is:
  //
  // Note that while this method returns an array of Constructor<T> objects (that is an array
  // of constructors from this class), the return type of this method is Constructor<?>[] and
  // not Constructor<T>[] as might be expected. This less informative return type is necessary
  // since after being returned from this method, the array could be modified to hold Constructor
  // objects for different classes, which would violate the type guarantees of Constructor<T>[]
  //
  // Since this reasoning is invalid in scala due to its abandonment of Array covariance,
  // these methods exist to correct the return types.
  //
  // In addition, at this writing because of ticket #1560 the compiler crashes on the
  // untyped constructors but not on these.

  def getConstructorsTyped(): Array[JConstructor[T]] =
    self.getConstructors() map (_.asInstanceOf[JConstructor[T]])

  def getDeclaredConstructorsTyped(): Array[JConstructor[T]] =
    self.getDeclaredConstructors() map (_.asInstanceOf[JConstructor[T]])

  private lazy val classLoader = self.getClassLoader match {
    case null   => getSystemLoader
    case x      => x
  }
  private val exceptions = List(
    classOf[ClassNotFoundException],
    classOf[NoSuchMethodException],
    classOf[SecurityException],
    classOf[NullPointerException],
    classOf[ClassCastException]
  )

  // Experimental!
  // scala> classOf[String].reflectiveCall[Array[String]]("ababab", "split")("b")
  // res0: Array[String] = Array(a, a, a)

  /** A class representing a reflective method call.  It is a function object
   *  and will make the call with whatever args are given via apply, or it will
   *  throw an exception at that point if there was an error in creation.
   */
  class ReflectiveCall[+U](obj: T, name: String) {
    def methodForArgs(args: AnyRef*) = self.getMethod(name, args map (_.getClass) : _*)
    def isErroneous = false
    def apply(args: Any*): U = {
      val ps = args map (_.asInstanceOf[AnyRef])
      val m = methodForArgs(ps: _*)
      m.invoke(obj, ps: _*).asInstanceOf[U]
    }
  }

  class FailedReflectiveCall[+U](ex: Throwable) extends ReflectiveCall[U](null.asInstanceOf[T], null) {
    override def isErroneous = true
    override def apply(args: Any*) = throw ex
  }

  def reflectiveCall[U](obj: T, method: String): ReflectiveCall[U] = {
    (catching(exceptions: _*) either (new ReflectiveCall[U](obj, method))) match {
      case Left(x)    => new FailedReflectiveCall[U](x)
      case Right(x)   => x
    }
  }
}

