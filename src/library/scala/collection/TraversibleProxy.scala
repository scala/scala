/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: IterableProxy.scala 15458 2008-06-28 20:23:22Z stepancheg $


package scala.collection

import generic._

// Methods could be printed by  cat TraversibeTemplate.scala | egrep '^  (override )?def'


/** This trait implements a proxy for traversible objects. It forwards
 *  all calls to a different traversible object
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait TraversibleProxy[+A] extends Traversible[A] with TraversibleProxyTemplate[A, Traversible[A]]
