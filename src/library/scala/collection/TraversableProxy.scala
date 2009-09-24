/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._

// Methods could be printed by  cat TraversibeTemplate.scala | egrep '^  (override )?def'


/** This trait implements a proxy for traversable objects. It forwards
 *  all calls to a different traversable object
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait TraversableProxy[+A] extends Traversable[A] with TraversableProxyTemplate[A, Traversable[A]]
