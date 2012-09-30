package scala.reflect
package api

/** A mirror-aware factory for types.
 *
 * In the reflection API, artifacts are specific to universes and
 * symbolic references used in artifacts (e.g. `scala.Int`) are resolved by mirrors.
 *
 * Therefore to build a type one needs to know a universe that the type is going to be bound to
 * and a mirror that is going to resolve symbolic references (e.g. to determine that `scala.Int`
 * points to a core class `Int` from scala-library.jar).
 *
 * `TypeCreator` implements this notion by providing a standalone type factory.
 *
 * This is immediately useful for type tags. When the compiler creates a type tag,
 * the end result needs to make sense in any mirror. That's because the compiler knows
 * the universe it's creating a type tag for (since `TypeTag` is path-dependent on a universe),
 * but it cannot know in advance the mirror to instantiate the result in (e.g. on JVM
 * it doesn't know what classloader use to resolve symbolic names in the type tag).
 *
 * Due to a typechecker restriction (no eta-expansion for dependent method types),
 * `TypeCreator` can't have a functional type, so it's implemented as class with an apply method.
 */
abstract class TypeCreator {
  def apply[U <: Universe with Singleton](m: scala.reflect.api.Mirror[U]): U # Type
}
