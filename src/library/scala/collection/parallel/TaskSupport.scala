/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel







trait TaskSupport extends Tasks

private[collection] class ForkJoinTaskSupport extends TaskSupport with AdaptiveWorkStealingForkJoinTasks

private[collection] class ThreadPoolTaskSupport extends TaskSupport with AdaptiveWorkStealingThreadPoolTasks


















