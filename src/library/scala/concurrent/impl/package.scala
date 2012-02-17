/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent



import java.{lang => jl}
import scala.util.Duration



package object impl {
  
  private val toBoxed = Map[Class[_], Class[_]](
    classOf[Boolean] -> classOf[jl.Boolean],
    classOf[Byte] -> classOf[jl.Byte],
    classOf[Char] -> classOf[jl.Character],
    classOf[Short] -> classOf[jl.Short],
    classOf[Int] -> classOf[jl.Integer],
    classOf[Long] -> classOf[jl.Long],
    classOf[Float] -> classOf[jl.Float],
    classOf[Double] -> classOf[jl.Double],
    classOf[Unit] -> classOf[scala.runtime.BoxedUnit])

  def boxedType(c: Class[_]): Class[_] = {
    if (c.isPrimitive) toBoxed(c) else c
  }  
  
  def dur2long(dur: Duration): Long = if (dur.isFinite) dur.toNanos else Long.MaxValue
  
}


