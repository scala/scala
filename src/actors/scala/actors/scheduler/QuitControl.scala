/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors.scheduler

import scala.util.control.ControlThrowable

/**
 * The `QuitControl` class is used to manage control flow of certain
 * schedulers.
 *
 * @author Philipp Haller
 */
private[scheduler] class QuitControl extends ControlThrowable
