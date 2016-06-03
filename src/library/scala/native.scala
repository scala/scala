/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** Marker for native methods.
  *
  * {{{
  * @native def f(x: Int, y: List[Long]): String = ...
  * }}}
  *
  * A `@native` method is compiled to the platform's native method,
  * while discarding the method's body (if any). The body will be type checked if present.
  *
  * A method marked @native must be a member of a class, not a trait (since 2.12).
  *
  * @since 2.6
  */
class native extends scala.annotation.StaticAnnotation {}
