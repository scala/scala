package scala.reflect
package api

/** A mirror-aware factory for trees.
 *
 * In the reflection API, artifacts are specific to universes and
 * symbolic references used in artifacts (e.g. `scala.Int`) are resolved by mirrors.
 *
 * Therefore to build a tree one needs to know a universe that the tree is going to be bound to
 * and a mirror that is going to resolve symbolic references (e.g. to determine that `scala.Int`
 * points to a core class `Int` from scala-library.jar).
 *
 * `TreeCreator` implements this notion by providing a standalone tree factory.
 *
 * This is immediately useful for reification. When the compiler reifies an expression,
 * the end result needs to make sense in any mirror. That's because the compiler knows
 * the universe it's reifying an expression into (specified by the target of the `reify` call),
 * but it cannot know in advance the mirror to instantiate the result in (e.g. on JVM
 * it doesn't know what classloader use to resolve symbolic names in the reifee).
 *
 * Due to a typechecker restriction (no eta-expansion for dependent method types),
 * `TreeCreator` can't have a functional type, so it's implemented as class with an apply method.
 */
abstract class TreeCreator {
  def apply[U <: Universe with Singleton](m: scala.reflect.api.Mirror[U]): U # Tree
}
