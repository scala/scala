/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
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
  * Method body is not generated if method is marked with `@native`,
  * but it is type checked when present.
  *
  * @since 2.6 */
class native extends annotation.StaticAnnotation {}
