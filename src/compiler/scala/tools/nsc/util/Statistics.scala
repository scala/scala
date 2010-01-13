/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package util

object Statistics {

  private var _enabled = false

  def enabled = _enabled
  def enabled_=(cond: Boolean) = {
    if (cond && !_enabled) {
      val test = new Timer()
      val start = System.nanoTime()
      var total = 0L
      for (i <- 1 to 10000) {
        val time = System.nanoTime()
        total += System.nanoTime() - time
      }
      val total2 = System.nanoTime() - start
      println("Enabling statistics, measuring overhead = "+
              total/10000.0+"ns to "+total2/10000.0+"ns per timer")
      _enabled = true
    }
  }

  var phasesShown = List("parser", "typer", "erasure", "cleanup")

  def currentTime() =
    if (_enabled) System.nanoTime() else 0L

  private def showPercent(x: Double, base: Double) =
    if (base == 0) "" else " ("+"%2.1f".format(x / base * 100)+"%)"

  def incCounter(c: Counter) {
    if (_enabled) c.value += 1
  }

  def incCounter(c: Counter, delta: Int) {
    if (_enabled) c.value += delta
  }

  def startCounter(sc: SubCounter): IntPair =
    if (_enabled) sc.start() else null

  def stopCounter(sc: SubCounter, start: IntPair) {
    if (_enabled) sc.stop(start)
  }

  def startTimer(tm: Timer): LongPair =
    if (_enabled) tm.start() else null

  def stopTimer(tm: Timer, start: LongPair) {
    if (_enabled) tm.stop(start)
  }

  case class IntPair(x: Int, y: Int)
  case class LongPair(x: Long, y: Long)

  class Counter {
    var value: Int = 0
    override def toString = value.toString
  }

  class SubCounter(c: Counter) {
    var value: Int = 0
    def start(): IntPair =
      if (_enabled) IntPair(value, c.value) else null
    def stop(prev: IntPair) {
      if (_enabled) {
        val IntPair(value0, cvalue0) = prev
        value = value0 + c.value - cvalue0
      }
    }
    override def toString =
      value+showPercent(value, c.value)
  }

  class Timer {
    var nanos: Long = 0
    var timings = 0
    def start(): LongPair =
      if (_enabled) {
        timings += 1
        LongPair(nanos, System.nanoTime())
      } else null
    def stop(prev: LongPair) {
      if (_enabled) {
        val LongPair(nanos0, start) = prev
        nanos = nanos0 + System.nanoTime() - start
        timings += 1
      }
    }
    override def toString = (timings/2)+" spans, "+nanos.toString+"ns"
  }

  class ClassCounts extends scala.collection.mutable.HashMap[Class[_], Int] {
    override def default(key: Class[_]) = 0
  }

  var nodeByType = new ClassCounts

  var microsByType = new ClassCounts
  var visitsByType = new ClassCounts
  var pendingTreeTypes: List[Class[_]] = List()
  var typerTime: Long = 0L

  val singletonBaseTypeSeqCount = new Counter
  val compoundBaseTypeSeqCount = new Counter
  val typerefBaseTypeSeqCount = new Counter
  val findMemberCount = new Counter
  val noMemberCount = new Counter
  val multMemberCount = new Counter
  val findMemberNanos = new Timer
  val asSeenFromCount = new Counter
  val asSeenFromNanos = new Timer
  val subtypeCount = new Counter
  val subtypeNanos = new Timer
  val sametypeCount = new Counter
  val rawTypeCount = new Counter
  val rawTypeFailed = new SubCounter(rawTypeCount)
  val findMemberFailed = new SubCounter(findMemberCount)
  val subtypeFailed = new SubCounter(subtypeCount)
  val rawTypeImpl = new SubCounter(rawTypeCount)
  val findMemberImpl = new SubCounter(findMemberCount)
  val subtypeImpl = new SubCounter(subtypeCount)
  val baseTypeSeqCount = new Counter
  val baseTypeSeqLenTotal = new Counter
  val typeSymbolCount = new Counter
  val classSymbolCount = new Counter
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
  val subtypeAppInfos = new SubCounter(subtypeCount)
  val subtypeImprovCount = new SubCounter(subtypeCount)
  val subtypeETNanos = new Timer
  val matchesPtNanos = new Timer
  val ctr1 = new Counter
  val ctr2 = new Counter
  val ctr3 = new Counter
  val counter1: SubCounter = new SubCounter(subtypeCount)
  val counter2: SubCounter = new SubCounter(subtypeCount)
  val timer1: Timer = new Timer
  val timer2: Timer = new Timer
}

abstract class Statistics {

  import Statistics._

  val global: Global
  import global._

  def countNodes(tree: Tree, counts: ClassCounts) {
    for (t <- tree) counts(t.getClass) += 1
    counts
  }

  def showRelative(base: Long)(value: Long) =
    value+showPercent(value, base)

  def showRelTyper(timer: Timer) =
    timer+showPercent(timer.nanos, typerNanos.nanos)

  def showCounts(counts: ClassCounts) =
    counts.toSeq.sortWith(_._2 > _._2).map {
      case (cls, cnt) =>
        cls.toString.substring(cls.toString.lastIndexOf("$") + 1)+": "+cnt
    }

  def print(phase: Phase) = if (phasesShown contains phase.name) {
    inform("*** Cumulative statistics at phase " + phase)
    inform("#created tree nodes  : " + nodeCount)
    inform("#created tree nodes by type: "+showCounts(nodeByType))
    if (phase.name != "parser") {
      val counts = new ClassCounts
      for (u <- currentRun.units; t <- u.body) counts(t.getClass) += 1
      inform("#retained nodes          : " + counts.valuesIterable.sum)
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
        inform("time spent typechecking  : "+showRelTyper(typerNanos))
        inform("time classfilereading    : "+showRelTyper(classReadNanos))
        inform("time spent in implicits  : "+showRelTyper(implicitNanos))
        inform("    successful in scope  : "+showRelTyper(inscopeSucceedNanos))
        inform("        failed in scope  : "+showRelTyper(inscopeFailNanos))
        inform("     successful of type  : "+showRelTyper(oftypeSucceedNanos))
        inform("         failed of type  : "+showRelTyper(oftypeFailNanos))
        inform("       assembling parts  : "+showRelTyper(subtypeETNanos))
        inform("              matchesPT  : "+showRelTyper(matchesPtNanos))
        inform("implicit cache hits      : "+showRelative(implicitCacheHits.value + implicitCacheMisses.value)(implicitCacheHits.value))
        inform("time spent in failed     : "+showRelTyper(failedSilentNanos))
        inform("       failed apply      : "+showRelTyper(failedApplyNanos))
        inform("       failed op=        : "+showRelTyper(failedOpEqNanos))
        inform("micros by tree node      : "+showCounts(microsByType))
        inform("#visits by tree node     : "+showCounts(visitsByType))
        val average = new ClassCounts
        for (c <- microsByType.keysIterator) average(c) = microsByType(c)/visitsByType(c)
        inform("avg micros by tree node  : "+showCounts(average))
        inform("time spent in <:<        : "+showRelTyper(subtypeNanos))
        inform("time spent in findmember : "+showRelTyper(findMemberNanos))
        inform("time spent in asSeenFrom : "+showRelTyper(asSeenFromNanos))
        inform("#implicit searches       : " + implicitSearchCount)
        inform("#tried, plausible, matching, typed, found implicits: "+triedImplicits+", "+plausiblyCompatibleImplicits+", "+matchingImplicits+", "+typedImplicits+", "+foundImplicits)
        inform("#implicit improves tests : " + improvesCount)
        inform("#implicit inscope hits   : " + inscopeImplicitHits)
        inform("#implicit oftype hits    : " + oftypeImplicitHits)
      }

      if (ctr1 != null)     inform("#ctr1                    : " + ctr1)
      if (ctr2 != null)     inform("#ctr2                    : " + ctr2)
      if (ctr3 != null)     inform("#ctr3                    : " + ctr3)
      if (counter1 != null) inform("#counter1                : " + counter1)
      if (counter2 != null) inform("#counter2                : " + counter2)
      if (timer1   != null) inform("#timer1                  : " + timer1)
      if (timer2   != null) inform("#timer2                  : " + timer2)
      //for (t <- uniques.iterator) println("unique: "+t)
    }
  }
}
