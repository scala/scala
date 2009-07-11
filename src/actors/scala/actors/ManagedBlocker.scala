/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import forkjoin.ForkJoinPool

/**
 * The <code>ManagedBlocker</code> trait...
 *
 * @author Philipp Haller
 */
trait ManagedBlocker extends ForkJoinPool.ManagedBlocker
