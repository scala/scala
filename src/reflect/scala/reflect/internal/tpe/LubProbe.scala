/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal
package tpe

import scala.collection.mutable

/** TEMPORARY instrumentation: detects re-entrant top-level lub/glb and the resulting
 *  sharing/clobbering of `lubResults`/`glbResults`. Enabled with `-Dscalac.lubprobe`.
 *  Not thread safe; only meaningful for the single-threaded compiler.
 */
private[internal] object LubProbe {
  final val enabled: Boolean = System.getProperty("scalac.lubprobe") != null
  /** Restore the pre-fix cache lifetime (clear in `finally`, shared across nesting). */
  final val legacy: Boolean = System.getProperty("scalac.lublegacy") != null
  private val maxStacks = Integer.getInteger("scalac.lubprobe.stacks", 5)
  private val stackDepth = Integer.getInteger("scalac.lubprobe.depth", 80)

  private var nextFrame = 0
  private var frames: List[Frame] = Nil
  final class Frame(val id: Int, val kind: String, val ts: List[AnyRef], val outerEntries: Int, val outerPending: Int)

  // key -> (owning frame id, pending)
  private val owner = mutable.HashMap[AnyRef, (Int, Boolean)]()

  val counts = mutable.LinkedHashMap[String, Long]().withDefaultValue(0L)
  private val stacksPrinted = mutable.HashMap[String, Int]().withDefaultValue(0)

  def nesting = frames.length

  private def event(name: String, detail: => String): Unit = {
    counts(name) += 1
    if (stacksPrinted(name) < maxStacks) {
      stacksPrinted(name) += 1
      val st = new Throwable().getStackTrace.drop(2).take(stackDepth).mkString("\n    at ")
      System.err.println(s"[lubprobe] $name #${counts(name)}: $detail\n    at $st")
    }
  }

  def enter(kind: String, ts: List[AnyRef], lubSize: Int, glbSize: Int): Unit = {
    counts(s"toplevel.$kind") += 1
    val pending = owner.valuesIterator.count(_._2)
    val f = new Frame(nextFrame, kind, ts, lubSize + glbSize, pending)
    nextFrame += 1
    if (frames.isEmpty && f.outerEntries > 0)
      event("outermost.staleEntries", s"$kind($ts) entered with ${f.outerEntries} entries left by non-top-level lub/glb")
    if (frames.nonEmpty) {
      counts("nested") += 1
      counts(s"nested.via.${classify()}") += 1
      if (f.outerEntries > 0) event("nested.withOuterEntries", s"$kind($ts) inside ${frames.head.kind}(${frames.head.ts}); outer entries=${f.outerEntries} pending=$pending")
    }
    frames = f :: frames
  }

  def exit(): Unit = {
    val f = frames.head
    frames = frames.tail
    if (frames.nonEmpty && f.outerEntries > 0) {
      counts("clear.clobberedOuterEntries") += 1
      if (f.outerPending > 0)
        event("clear.clobberedOuterPlaceholder", s"${f.kind}(${f.ts}) cleared ${f.outerPending} in-progress placeholder(s) of ${frames.head.kind}(${frames.head.ts})")
    }
    owner.clear()
  }

  /** What lies between this top-level entry and the enclosing lub/glb frame? */
  private def classify(): String = {
    val st = new Throwable().getStackTrace.toList.drop(3)
    val seg = st.drop(1).takeWhile(e => !e.getClassName.contains("GlbLubs"))
    def has(f: StackTraceElement => Boolean) = seg.exists(f)
    if (has(_.getMethodName == "completeInfo")) "completer"
    else if (has(_.getMethodName == "solve")) "solve"
    else if (has(_.getMethodName.contains("annotationsLub"))) "annotationsLub"
    else if (has(e => e.getMethodName.contains("mergePrefixAndArgs"))) "mergePrefixAndArgs"
    else "other:" + seg.take(3).map(e => e.getClassName.split('.').last + "." + e.getMethodName).mkString("<")
  }

  private def curId = if (frames.isEmpty) -1 else frames.head.id

  def hit(kind: String, key: AnyRef): Unit = owner.get(key) match {
    case Some((id, pending)) if id != curId =>
      if (pending) event(s"$kind.hitOuterPlaceholder", s"key=$key")
      else counts(s"$kind.hitOuterResult") += 1
    case _ =>
  }
  def placeholder(key: AnyRef): Unit = owner(key) = (curId, true)
  def result(kind: String, key: AnyRef, stillPresent: Boolean): Unit = {
    if (!stillPresent) event(s"$kind.placeholderLost", s"key=$key")
    owner(key) = (curId, false)
  }

  if (enabled) Runtime.getRuntime.addShutdownHook(new Thread(() =>
    System.err.println("[lubprobe] summary: " + counts.map { case (k, v) => s"$k=$v" }.mkString(", "))))
}
