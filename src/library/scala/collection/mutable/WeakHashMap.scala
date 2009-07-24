/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Symbol.scala 17537 2009-04-20 18:37:37Z odersky $


package scala.collection.mutable

import scala.collection.JavaConversions._

class WeakHashMap[A, B] extends JMapWrapper[A, B](new java.util.WeakHashMap) {
  override def empty = new WeakHashMap[A, B]
}
