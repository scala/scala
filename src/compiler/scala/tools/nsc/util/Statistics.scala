/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.util

object Statistics {
  final val enabled = false
}

abstract class Statistics {

  val global: Global
  import global._

  def print(phase: Phase) = {
    inform("*** Cumulative statistics at phase " + phase)
    inform("#tree nodes  : " + nodeCount)
    inform("#identifiers : " + analyzer.idcnt)
    inform("#selections  : " + analyzer.selcnt)
    inform("#applications: " + analyzer.appcnt)
    inform("#implicits   : " + analyzer.implcnt)
    inform("ms implicits : " + analyzer.impltime)
    inform("#uniquetypes : " + uniqueTypeCount)
    inform("#symbols     : " + symbolCount)
    inform("#type symbols: " + typeSymbolCount)
    inform("#class symbols: " + classSymbolCount)
    inform("#singleton closures: " + singletonBaseTypeSeqCount)
    inform("#compound closures : " + compoundBaseTypeSeqCount)
    inform("#typeref closures  : " + typerefBaseTypeSeqCount)
    inform("#findMember     : " + findMemberCount)
    inform("#notfound member: " + noMemberCount)
    inform("#mulitple member: " + multMemberCount)
    inform("time findMember: " + findMemberMillis)
    inform("#norm meth : " + analyzer.normM)
    inform("#norm poly : " + analyzer.normP)
    inform("#norm other: " + analyzer.normO)
    inform("#subtype  : " + subtypeCount)
    inform("ms subtype: " + subtypeMillis)
    inform("ms type-flow-analysis: " + analysis.timer.millis)
  }
}
