/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: DefaultMapModel.scala 9554 2007-01-04 16:30:16 +0000 (Thu, 04 Jan 2007) odersky $


package scala.collection.mutable

import Predef._

@serializable
final class DefaultEntry[A, B](val key: A, var value: B)
      extends HashEntry[A, DefaultEntry[A, B]]
