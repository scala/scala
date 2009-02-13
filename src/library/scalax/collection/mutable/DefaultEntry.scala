/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: DefaultEntry.scala 16893 2009-01-13 13:09:22Z cunei $


package scalax.collection.mutable

@serializable
final class DefaultEntry[A, B](val key: A, var value: B)
      extends HashEntry[A, DefaultEntry[A, B]]
