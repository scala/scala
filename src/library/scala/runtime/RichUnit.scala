/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: RichInt.scala 14532 2008-04-07 12:23:22Z washburn $


package scala.runtime

/** This class exists only as a dummy subclass so that there are two ambiguous
 *  implicit conversions from Unit to some subclass to Object.
 *  It's important that this class should NOT inherit from Ordered
 */
final class RichUnit {}
