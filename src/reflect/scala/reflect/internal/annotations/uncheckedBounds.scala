package scala.reflect
package internal
package annotations

/**
 * An annotation that designates the annotated type should not be checked for violations of
 * type parameter bounds in the `refchecks` phase of the compiler. This can be used by synthesized
 * code the uses an inferred type of an expression as the type of an artifact val/def (for example,
 * a temporary value introduced by an ANF transform). See [[https://issues.scala-lang.org/browse/SI-7694]].
 *
 * @since  2.10.3
 */
final class uncheckedBounds extends scala.annotation.StaticAnnotation
