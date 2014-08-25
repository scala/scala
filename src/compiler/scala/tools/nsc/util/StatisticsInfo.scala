/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package util

import scala.reflect.internal.util.Statistics

abstract class StatisticsInfo {

  val global: Global
  import global._
  import scala.reflect.internal.TreesStats.nodeByType

  val retainedCount  = Statistics.newCounter("#retained tree nodes")
  val retainedByType = Statistics.newByClass("#retained tree nodes by type")(Statistics.newCounter(""))

  def print(phase: Phase) = if (settings.Ystatistics contains phase.name) {
    inform("*** Cumulative statistics at phase " + phase)
    retainedCount.value = 0
    for (c <- retainedByType.keys)
      retainedByType(c).value = 0
    for (u <- currentRun.units; t <- u.body) {
      retainedCount.value += 1
      retainedByType(t.getClass).value += 1
    }

    val quants =
      if (phase.name == "parser") Seq(treeNodeCount, nodeByType, retainedCount, retainedByType)
      else Statistics.allQuantities

    for (q <- quants if q.showAt(phase.name)) inform(q.line)
  }
}
