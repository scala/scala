package scala.annotation

/**
 * This annotation, used for two-parameter generic types makes Scala print
 * the type using infix notation:
 *
 * ```
 * scala> class &&[T, U]
 * defined class $amp$amp
 *
 * scala> def foo: Int && Int = ???
 * foo: &&[Int,Int]
 *
 * scala> @showAsInfix class &&[T, U]
 * defined class $amp$amp
 *
 * scala> def foo: Int && Int = ???
 * foo: Int && Int
 * ```
 */
class showAsInfix extends annotation.StaticAnnotation