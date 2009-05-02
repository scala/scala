/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: NonLocalReturnException.scala 15604 2008-07-24 09:56:59Z phaller $


package scala.runtime


import Predef.RuntimeException

class NonLocalReturnException[T](val key: AnyRef, val value: T) extends RuntimeException