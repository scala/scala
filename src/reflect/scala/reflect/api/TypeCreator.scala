package scala
package reflect
package api

/** A mirror-aware factory for types.
 *
 * This class is used internally by Scala Reflection, and is not recommended for use in client code.
 *
 * @group ReflectionAPI
 */
abstract class TypeCreator extends Serializable {
  def apply[U <: Universe with Singleton](m: scala.reflect.api.Mirror[U]): U # Type
}
