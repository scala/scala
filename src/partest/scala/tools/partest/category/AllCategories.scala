/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools
package partest
package category

trait AllCategories extends Compiler with Analysis with Runner {
  self: Universe =>

  object Pos extends DirBasedCategory("pos") { lazy val testSequence: TestSequence = List(compile) }
  object Neg extends DirBasedCategory("neg") { lazy val testSequence: TestSequence = List(isCheckPresent, not(compile), diff) }
  object Run extends DirBasedCategory("run") { lazy val testSequence: TestSequence = List(compile, run, diff) }
  object Jvm extends DirBasedCategory("jvm") { lazy val testSequence: TestSequence = List(compile, run, diff) }
}
