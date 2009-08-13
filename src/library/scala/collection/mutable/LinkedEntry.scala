/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: DefaultEntry.scala 17680 2009-05-08 16:33:15Z odersky $


package scala.collection.mutable

@serializable
final class LinkedEntry[A, B](val key: A, var value: B)
      extends HashEntry[A, LinkedEntry[A, B]] {
  var earlier: LinkedEntry[A, B] = null
  var later: LinkedEntry[A, B] = null
}

