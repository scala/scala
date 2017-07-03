package scala.tools.nsc.profile

import java.io.{FileWriter, PrintWriter}
import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.AtomicInteger

import scala.tools.nsc.{Phase, Settings}

object Profiler {
  def apply(settings: Settings): Profiler =
    if (!settings.YprofileEnabled) NoOpProfiler
    else {
      val reporter = if (settings.YprofileDestination.isSetByUser)
        new StreamProfileReporter(new PrintWriter(new FileWriter(settings.YprofileDestination.value, true)))
      else ConsoleProfileReporter
      new RealProfiler(reporter, settings)
    }
}


case class ProfileCounters(wallClockTimeNanos: Long, cpuTimeNanos: Long, userTimeNanos: Long, allocatedBytes: Long,
                           retainedHeapBytes: Long, gcTimeMillis: Long) {
  def -(that: ProfileCounters): ProfileCounters = {
    ProfileCounters(
      this.wallClockTimeNanos - that.wallClockTimeNanos,
      this.cpuTimeNanos - that.cpuTimeNanos,
      this.userTimeNanos - that.userTimeNanos,
      this.allocatedBytes - that.allocatedBytes,
      this.retainedHeapBytes - that.retainedHeapBytes,
      this.gcTimeMillis - that.gcTimeMillis)
  }
  def updateHeap(heapDetails: ProfileCounters): ProfileCounters = {
    copy(retainedHeapBytes = heapDetails.retainedHeapBytes)
  }
  private def toMillis(ns: Long) = ns / 1000000.0D
  private def toMegaBytes(bytes: Long) = bytes / 1000000.0D

  def wallClockTimeMillis: Double = toMillis(wallClockTimeNanos)
  def cpuTimeMillis: Double = toMillis(cpuTimeNanos)
  def userTimeMillis: Double = toMillis(userTimeNanos)
  def allocatedMB: Double = toMegaBytes(allocatedBytes)
  def retainedHeapMB: Double = toMegaBytes(retainedHeapBytes)

}

sealed trait Profiler {
  def finished(): Unit

  def after(phase: Phase, profileBefore: ProfileCounters): Unit

  def before(phase: Phase): ProfileCounters

}

private[profile] object NoOpProfiler extends Profiler {
  val noSnap = ProfileCounters(0, 0, 0, 0, 0, 0)

  override def before(phase: Phase): ProfileCounters = noSnap

  override def after(phase: Phase, profileBefore: ProfileCounters): Unit = ()

  override def finished(): Unit = ()
}

private[profile] object RealProfiler {
  import scala.collection.JavaConverters._
  private val runtimeMx = ManagementFactory.getRuntimeMXBean
  private val memoryMx = ManagementFactory.getMemoryMXBean
  private val gcMx = ManagementFactory.getGarbageCollectorMXBeans.asScala.toList
  private val threadMx = ExtendedThreadMxBean.proxy
  if (threadMx.isThreadCpuTimeSupported) threadMx.setThreadCpuTimeEnabled(true)
  private val idGen = new AtomicInteger()
}

private[profile] class RealProfiler(reporter: ProfileReporter, val settings: Settings) extends Profiler {
  def outDir: String = settings.outdir.value
  val id: Int = RealProfiler.idGen.incrementAndGet()

  private def snap: ProfileCounters = {
    import RealProfiler._
    ProfileCounters(System.nanoTime(), threadMx.getCurrentThreadCpuTime, threadMx.getCurrentThreadUserTime,
      threadMx.getThreadAllocatedBytes(Thread.currentThread().getId), memoryMx.getHeapMemoryUsage.getUsed,
      gcMx.foldLeft(0L) { case (sum, bean) => bean.getCollectionTime + sum })
  }
  private val overhead = {
    val s1 = snap
    val s2 = snap
    s2 - s1
  }
  private def doGC(): Unit = {
    System.gc()
    System.runFinalization()
  }
  reporter.header(this)

  override def finished(): Unit = reporter.close(this)

  override def after(phase: Phase, profileBefore: ProfileCounters): Unit = {
    val initialSnap = snap
    if (settings.YprofileExternalTool.containsPhase(phase)) {
      println("Profile hook stop")
      ExternalToolHook.after()
    }
    val finalSnap = if (settings.YprofileRunGcBetweenPhases.containsPhase(phase)) {
      doGC()
      initialSnap.updateHeap(snap)
    } else initialSnap
    reporter.report(this, phase, finalSnap - profileBefore - overhead)
  }
  override def before(phase: Phase): ProfileCounters = {
    if (settings.YprofileRunGcBetweenPhases.containsPhase(phase))
      doGC()
    if (settings.YprofileExternalTool.containsPhase(phase)) {
      println("Profile hook start")
      ExternalToolHook.before()
    }

    snap
  }
}

sealed trait ProfileReporter {
  def report(profiler: RealProfiler, phase: Phase, diff: ProfileCounters): Unit
  def header(profiler: RealProfiler): Unit
  def close(profiler: RealProfiler): Unit
}

object ConsoleProfileReporter extends ProfileReporter {
  override def report(profiler: RealProfiler, phase: Phase, diff: ProfileCounters): Unit =
    println(f"Profiler compile ${profiler.id} after phase ${phase.id}%2d:${phase.name}%20s wallClockTime: ${diff.wallClockTimeMillis}%12.4fms, cpuTime ${diff.cpuTimeMillis}%12.4fms, userTime ${diff.userTimeMillis}%12.4fms, allocatedBytes ${diff.allocatedMB}%12.4fMB, retainedHeapBytes ${diff.retainedHeapMB}%12.4fMB, gcTime ${diff.gcTimeMillis}%6dms")

  override def close(profiler: RealProfiler): Unit = ()

  override def header(profiler: RealProfiler): Unit = {
    println(s"Profiler start (${profiler.id}) ${profiler.outDir}")
  }
}

class StreamProfileReporter(out: PrintWriter) extends ProfileReporter {
  override def header(profiler: RealProfiler): Unit = {
    out.println(s"info, ${profiler.id}, ${profiler.outDir}")
    out.println(s"header,id,phaseId,phaseName,wallClockTimeMs,cpuTimeMs,userTimeMs,allocatedMB,retainedHeapMB,gcTimeMs")
  }
  override def report(profiler: RealProfiler, phase: Phase, diff: ProfileCounters): Unit = {
    out.println(s"data,${profiler.id},${phase.id},${phase.name},${diff.wallClockTimeMillis},${diff.cpuTimeMillis},${diff.userTimeMillis},${diff.allocatedMB},${diff.retainedHeapMB},${diff.gcTimeMillis}")
  }

  override def close(profiler: RealProfiler): Unit = {
    out.flush()
    out.close()
  }
}

