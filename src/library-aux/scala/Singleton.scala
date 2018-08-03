/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** Scala has concept of object.An object is a class that has exactly one instance. It is created lazily.
 *
 *  A singleton type is of the form p.type, where p is a path pointing to a value expected to conform to scala.AnyRef. 
 *  The type denotes the set of values consisting of null and the value denoted by p.
 *
 */
final trait Singleton extends Any
