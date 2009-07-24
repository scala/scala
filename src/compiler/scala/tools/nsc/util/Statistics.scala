/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package util

object Statistics {
  var enabled = false
}

abstract class Statistics {

  val global: Global
  import global._

  def showRelative(base: Long)(time: Long) = "%2.1f".format(time.toDouble / base * 100)+" / "+time+"ns"
  def showRelTyper(time: Long) = showRelative(analyzer.typerTime)(time)

  def print(phase: Phase) = {
    if (List("typer", "erasure", "cleanup") contains phase.name) {
      inform("*** Cumulative statistics at phase " + phase)
      inform("#tree nodes  : " + nodeCount)
      inform("#identifiers : " + analyzer.idcnt)
      inform("#selections  : " + analyzer.selcnt)
      inform("#applications: " + analyzer.appcnt)
      inform("#implicits   : " + analyzer.implcnt)
      inform("#uniquetypes : " + uniqueTypeCount)
      inform("#symbols     : " + symbolCount)
      inform("#type symbols: " + typeSymbolCount)
      inform("#class symbols: " + classSymbolCount)
      inform("#singleton closures: " + singletonBaseTypeSeqCount)
      inform("#compound closures : " + compoundBaseTypeSeqCount)
      inform("#typeref closures  : " + typerefBaseTypeSeqCount)
      inform("#findMember     : " + findMemberCount)
      inform("#notfound member: " + noMemberCount)
      inform("#multiple member: " + multMemberCount)
      inform("time findMember: " + findMemberNanos)
      inform("#norm meth   : " + analyzer.normM)
      inform("#norm poly   : " + analyzer.normP)
      inform("#norm other  : " + analyzer.normO)
      inform("#subtype     : " + subtypeCount)
      inform("ns subtype   : " + subtypeNanos)
      inform("#sametype    : " + sametypeCount)
      inform("#unique types: " + uniques.size)
      inform("ms type-flow-analysis: " + analysis.timer.millis)
      if (phase.name == "typer") {
        inform("time spent typechecking: "+showRelTyper(analyzer.typerTime))
        inform("time spent in implicits: "+showRelTyper(analyzer.implicitTime))
        inform("    successful in scope: "+showRelTyper(analyzer.inscopeSucceed))
        inform("        failed in scope: "+showRelTyper(analyzer.inscopeFail))
        inform("     successful of type: "+showRelTyper(analyzer.oftypeSucceed))
        inform("         failed of type: "+showRelTyper(analyzer.oftypeFail))
        inform("    successful manifest: "+showRelTyper(analyzer.manifSucceed))
        inform("        failed manifest: "+showRelTyper(analyzer.manifFail))
        inform("implicit cache hitratio: "+"%2.1f".format(analyzer.hits.toDouble / (analyzer.hits + analyzer.misses) * 100))
        inform("implicit cache coverage: "+"%2.1f".format((analyzer.hits + analyzer.misses).toDouble / (analyzer.uncached + analyzer.hits + analyzer.misses) * 100))
        inform("time spent in failed   : "+showRelTyper(analyzer.failedSilent))
        inform("       failed op=      : "+showRelTyper(analyzer.failedOpEqs))
        inform("       failed apply    : "+showRelTyper(analyzer.failedApplies))
      }
      //for (t <- uniques.iterator) println("unique: "+t)
    }
  }
}
