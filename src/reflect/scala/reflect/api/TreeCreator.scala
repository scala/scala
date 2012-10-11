package scala.reflect
package api

/** This is an internal implementation class.
 *
 * This class is used internally by Scala Reflection, and is not recommended for use in client code.
 */
abstract class TreeCreator {
  def apply[U <: Universe with Singleton](m: scala.reflect.api.Mirror[U]): U # Tree
}
