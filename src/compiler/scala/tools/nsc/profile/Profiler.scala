package scala.tools.nsc.profile

import java.io.{FileWriter, Writer}
import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.AtomicInteger

import scala.tools.nsc.{Phase, Settings}

object Profiler {
  def apply(settings: Settings):Profiler =
    if (!settings.YprofileEnabled) NoOpProfiler
    else {
      val reporter = if(settings.YprofileDestination.isSetByUser)
        new StreamProfileReporter(new FileWriter(settings.YprofileDestination.value, true))
      else ConsoleProfileReporter
      new RealProfiler(reporter, settings.outdir.value)
    }
}


case class ProfileCounters(wallClockTimeNs : Long, cpuTimeNs: Long, userTimeNs: Long, allocatedBytes:Long ) {
  def - (that :ProfileCounters) = {
    ProfileCounters(
      this.wallClockTimeNs - that.wallClockTimeNs,
      this.cpuTimeNs - that.cpuTimeNs,
      this.userTimeNs - that.userTimeNs,
      this.allocatedBytes - that.allocatedBytes)
  }
}

sealed trait Profiler {
  def finished() :Unit

  def after(phase: Phase, profileBefore: ProfileCounters) : Unit

  def before(phase: Phase) : ProfileCounters

}
private [profile] object NoOpProfiler extends Profiler {
  val noSnap = ProfileCounters(0,0,0,0)

  override def before(phase: Phase): ProfileCounters = noSnap

  override def after(phase: Phase, profileBefore: ProfileCounters): Unit = ()

  override def finished(): Unit = ()
}
private [profile] object RealProfiler {
  val runtimeMx = ManagementFactory.getRuntimeMXBean
  val memoryMx = ManagementFactory.getMemoryMXBean
  val threadMx = ExtendedThreadMxBean.proxy
  if (threadMx.isThreadCpuTimeSupported) threadMx.setThreadCpuTimeEnabled(true)
  private val idGen = new AtomicInteger()
}

private [profile] class RealProfiler(reporter : ProfileReporter, val outDir:String) extends Profiler {

  val id = RealProfiler.idGen.incrementAndGet()

  private def snap :ProfileCounters = {
    import RealProfiler._
    ProfileCounters(System.nanoTime(), threadMx.getCurrentThreadCpuTime, threadMx.getCurrentThreadUserTime,
      threadMx.getThreadAllocatedBytes(Thread.currentThread().getId))
  }
  private val overhead = {
    val s1 = snap
    val s2 = snap
    s2 - s1
  }
  reporter.header(this)

  override def finished(): Unit = reporter.close(this)

  override def after(phase: Phase, profileBefore: ProfileCounters): Unit =
    reporter.report(this, phase, snap - profileBefore - overhead)

  override def before(phase: Phase): ProfileCounters = snap
}


sealed trait ProfileReporter {
  def report(profiler: RealProfiler, phase: Phase, diff: ProfileCounters) : Unit

  def header(profiler: RealProfiler) :Unit
  def close(profiler: RealProfiler) :Unit
}

object ConsoleProfileReporter extends ProfileReporter {
  override def report(profiler: RealProfiler, phase: Phase, diff: ProfileCounters): Unit =
    println(s"Profiler compile ${profiler.id} after phase ${phase.id}:(${phase.name}) wallClockTime: ${diff.wallClockTimeNs}ns, cpuTime ${diff.cpuTimeNs}, userTime ${diff.userTimeNs}, allocatedBytes ${diff.allocatedBytes}")

  override def close(profiler: RealProfiler): Unit = ()
  override def header(profiler: RealProfiler): Unit = ()
}

class StreamProfileReporter(out:Writer) extends ProfileReporter {
  override def header(profiler: RealProfiler): Unit = {
    out.write(s"info, ${profiler.id}, ${profiler.outDir}\n")
    out.write(s"header, id, phase, wallClockTimeNs, cpuTimeNs, userTimeNs, allocatedBytes\n")

  }
  override def report(profiler: RealProfiler, phase: Phase, diff: ProfileCounters): Unit = {
    out.write(s"data,${phase.id},${diff.wallClockTimeNs},${diff.cpuTimeNs},${diff.userTimeNs},${diff.allocatedBytes}\n")

  }

  override def close(profiler: RealProfiler): Unit = {
    out.flush
    out.close
  }
}

