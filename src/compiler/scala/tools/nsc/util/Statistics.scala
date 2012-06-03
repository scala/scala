/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package util

class Statistics extends scala.reflect.internal.util.Statistics {

 var nodeByType = new ClassCounts

  var microsByType = new ClassCounts
  var visitsByType = new ClassCounts
  var pendingTreeTypes: List[Class[_]] = List()
  var typerTime: Long = 0L

  val typedApplyCount = new Counter
  val typedIdentCount = new Counter
  val typedSelectCount = new Counter
  val typerNanos = new Timer
  val classReadNanos = new Timer

  val failedApplyNanos = new Timer
  val failedOpEqNanos = new Timer
  val failedSilentNanos = new Timer

  val implicitSearchCount = new Counter
  val implicitNanos = new Timer
  val oftypeImplicitHits = new Counter
  val inscopeImplicitHits = new Counter

  val triedImplicits = new Counter
  val plausiblyCompatibleImplicits = new Counter
  val matchingImplicits = new Counter
  val typedImplicits = new Counter
  val foundImplicits = new Counter

  val inscopeSucceedNanos = new Timer
  val inscopeFailNanos = new Timer
  val oftypeSucceedNanos = new Timer
  val oftypeFailNanos = new Timer
  val implicitCacheHits = new Counter
  val implicitCacheMisses = new Counter
  val improvesCount = new Counter
  val improvesCachedCount = new Counter
  val subtypeAppInfos = new SubCounter(subtypeCount)
  val subtypeImprovCount = new SubCounter(subtypeCount)
  val subtypeETNanos = new Timer
  val matchesPtNanos = new Timer
  val isReferencedNanos = new Timer
  val ctr1 = new Counter
  val ctr2 = new Counter
  val ctr3 = new Counter
  val ctr4 = new Counter
  val counter1: SubCounter = new SubCounter(subtypeCount)
  val counter2: SubCounter = new SubCounter(subtypeCount)
  val timer1: Timer = new Timer
  val timer2: Timer = new Timer

  val macroExpandCount = new Counter
  val macroExpandNanos = new Timer

  val patmatNanos      = new Timer
  val patmatAnaDPLL    = new Timer
  val patmatAnaVarEq   = new Timer
  val patmatCNF        = new Timer
  val patmatAnaExhaust = new Timer
  val patmatAnaReach   = new Timer
  val patmatCNFSizes   = new collection.mutable.HashMap[Int, Int] withDefaultValue 0
}

object Statistics extends Statistics

abstract class StatisticsInfo {

  import Statistics._

  val global: Global
  import global._

  var phasesShown = List("parser", "typer", "patmat", "erasure", "cleanup")

  def countNodes(tree: Tree, counts: ClassCounts) {
    for (t <- tree) counts(t.getClass) += 1
  }

  def showRelative(base: Long)(value: Long) =
    value+showPercent(value, base)

  def showRelTyper(timer: Timer) =
    timer+showPercent(timer.nanos, typerNanos.nanos)

  def showRelPatmat(timer: Timer) =
    timer+showPercent(timer.nanos, patmatNanos.nanos)

  def showCounts[T](counts: scala.collection.mutable.Map[T, Int]) =
    counts.toSeq.sortWith(_._2 > _._2).map {
      case (cls: Class[_], cnt) =>
        cls.toString.substring(cls.toString.lastIndexOf("$") + 1)+": "+cnt
      case (o, cnt) =>
        o.toString +": "+cnt
    }

  def print(phase: Phase) = if (phasesShown contains phase.name) {
    inform("*** Cumulative statistics at phase " + phase)
    inform("#created tree nodes  : " + nodeCount)
    inform("#created tree nodes by type: "+showCounts(nodeByType))
    if (phase.name != "parser") {
      val counts = new ClassCounts
      for (u <- currentRun.units; t <- u.body) counts(t.getClass) += 1
      inform("#retained nodes          : " + counts.values.sum)
      inform("#retained nodes by type  : " + showCounts(counts))
      inform("#typechecked identifiers : " + typedIdentCount)
      inform("#typechecked selections  : " + typedSelectCount)
      inform("#typechecked applications: " + typedApplyCount)
      inform("#raw type creations      : " + rawTypeCount)
      inform("  of which in failed     : " + rawTypeFailed)
      inform("  of which in implicits  : " + rawTypeImpl)
      inform("#unique types            : " + uniqueTypeCount)
      inform("#symbols                 : " + symbolCount)
      inform("  of which type symbols  : " + typeSymbolCount)
      inform("  of which class symbols : " + classSymbolCount)
      inform("#base type seqs          : " + baseTypeSeqCount)
      inform("avg base type seq length : " + baseTypeSeqLenTotal.value.toFloat / baseTypeSeqCount.value)
      inform("#singleton base type seqs: " + singletonBaseTypeSeqCount)
      inform("#compound base type seqs : " + compoundBaseTypeSeqCount)
      inform("#typeref base type seqs  : " + typerefBaseTypeSeqCount)
      inform("#findMember ops          : " + findMemberCount)
      inform("  of which in failed     : " + findMemberFailed)
      inform("  of which in implicits  : " + findMemberImpl)
      inform("#notfound member         : " + noMemberCount)
      inform("#multiple member         : " + multMemberCount)
      inform("#asSeenFrom ops          : " + asSeenFromCount)
      inform("#subtype                 : " + subtypeCount)
      inform("  of which in failed     : " + subtypeFailed)
      inform("  of which in implicits  : " + subtypeImpl)
      inform("  of which in app impl   : " + subtypeAppInfos)
      inform("  of which in improv     : " + subtypeImprovCount)
      inform("#sametype                : " + sametypeCount)
      inform("ms type-flow-analysis: " + analysis.timer.millis)

      if (phase.name == "typer") {
        inform("time spent typechecking    : " + showRelTyper(typerNanos))
        inform("time classfilereading      : " + showRelTyper(classReadNanos))
        inform("time spent in implicits    : " + showRelTyper(implicitNanos))
        inform("    successful in scope    : " + showRelTyper(inscopeSucceedNanos))
        inform("        failed in scope    : " + showRelTyper(inscopeFailNanos))
        inform("     successful of type    : " + showRelTyper(oftypeSucceedNanos))
        inform("         failed of type    : " + showRelTyper(oftypeFailNanos))
        inform("       assembling parts    : " + showRelTyper(subtypeETNanos))
        inform("              matchesPT    : " + showRelTyper(matchesPtNanos))
        inform("implicit cache hits        : " + showRelative(implicitCacheHits.value + implicitCacheMisses.value)(implicitCacheHits.value))
        inform("time spent in failed       : " + showRelTyper(failedSilentNanos))
        inform("       failed apply        : " + showRelTyper(failedApplyNanos))
        inform("       failed op=          : " + showRelTyper(failedOpEqNanos))
        inform("time spent ref scanning    : " + showRelTyper(isReferencedNanos))
        inform("micros by tree node        : " + showCounts(microsByType))
        inform("#visits by tree node       : " + showCounts(visitsByType))
        val average = new ClassCounts
        for (c <- microsByType.keysIterator) average(c) = microsByType(c)/visitsByType(c)
        inform("avg micros by tree node    : " + showCounts(average))
        inform("time spent in <:<          : " + showRelTyper(subtypeNanos))
        inform("time spent in findmember   : " + showRelTyper(findMemberNanos))
        inform("time spent in asSeenFrom   : " + showRelTyper(asSeenFromNanos))
        inform("#implicit searches         : " + implicitSearchCount)
        inform("#tried, plausible, matching, typed, found implicits: "+triedImplicits+", "+plausiblyCompatibleImplicits+", "+matchingImplicits+", "+typedImplicits+", "+foundImplicits)
        inform("#implicit improves tests   : " + improvesCount)
        inform("#implicit improves cached  : " + improvesCachedCount)
        inform("#implicit inscope hits     : " + inscopeImplicitHits)
        inform("#implicit oftype hits      : " + oftypeImplicitHits)
        inform("#macro expansions          : " + macroExpandCount)
        inform("#time spent in macroExpand : " + showRelTyper(macroExpandNanos))
      }

      if (ctr1 != null)     inform("#ctr1                    : " + ctr1)
      if (ctr2 != null)     inform("#ctr2                    : " + ctr2)
      if (ctr3 != null)     inform("#ctr3                    : " + ctr3)
      if (ctr4 != null)     inform("#ctr4                    : " + ctr4)
      if (counter1 != null) inform("#counter1                : " + counter1)
      if (counter2 != null) inform("#counter2                : " + counter2)
      if (timer1   != null) inform("#timer1                  : " + timer1)
      if (timer2   != null) inform("#timer2                  : " + timer2)
      //for (t <- uniques.iterator) println("unique: "+t)

      if (phase.name == "patmat") {
        inform("time spent in patmat       : " + patmatNanos                   )
        inform("             of which DPLL : " + showRelPatmat(patmatAnaDPLL   ))
        inform("of which in CNF conversion : " + showRelPatmat(patmatCNF       ))
        inform("           CNF size counts : " + showCounts(patmatCNFSizes     ))
        inform("of which variable equality : " + showRelPatmat(patmatAnaVarEq  ))
        inform("  of which in exhaustivity : " + showRelPatmat(patmatAnaExhaust))
        inform("of which in unreachability : " + showRelPatmat(patmatAnaReach  ))
      }
    }
  }
}
