/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** `Equality` is a trait whose instances each represent a strategy for determining
  * equality of instances of a type.
  *
  * `Equality`'s companion object defines a default equality for all
  * objects - it calls their `==` method.
  *
  * @since 2.10
  */
@annotation.implicitNotFound(msg = "No implicit Equality defined for ${T}.")
trait Equality[T] extends Serializable {
  
  def areEqual(x: T, y: T): Boolean
  
}


object Equality {
  
  implicit def defaultEquality[T] = new Equality[T] {
    def areEqual(x: T, y: T) = x == y
  }
  
  def apply[T](f: (T, T) => Boolean) = new Equality[T] {
    def areEqual(x: T, y: T) = f(x, y)
  }
  
}
