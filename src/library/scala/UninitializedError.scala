/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** This class represents uninitialized variable/value errors.
 *
 *  @author  Martin Odersky
 *  @since   2.5
 */
// TODO: remove in 2.14
@deprecated("will be removed in a future release", since = "2.12.7")
final class UninitializedError extends RuntimeException("uninitialized value")
