package scala.reflect
package api

/** This is an internal implementation class.
 * 
 * @see [[http://docs.scala-lang.org/overviews/reflection/architecture.html]].
 */
abstract class TypeCreator {
  def apply[U <: Universe with Singleton](m: scala.reflect.api.Mirror[U]): U # Type
}
