/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/**
 * Marker for native methods.
 * <p>
 *   <code>@native def f(x: Int, y: List[Long]): String = ..</code>
 * </p>
 * <p>
 *   Method body is not generated if method is marked with <code>@native</code>,
 *   but it is type checked when present.
 * </p>
 *
 * @since 2.6
 */
class native extends StaticAnnotation {}
