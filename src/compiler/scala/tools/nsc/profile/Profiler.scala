package scala.tools.nsc.profile

import java.io.{FileWriter, PrintWriter}
import java.lang.management.ManagementFactory
import java.util.concurrent.atomic.AtomicInteger
import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}

import scala.collection.mutable
import scala.tools.nsc.{Phase, Settings}

object Profiler {
  def apply(settings: Settings):Profiler =
    if (!settings.YprofileEnabled) NoOpProfiler
    else {
      val reporter = if(settings.YprofileDestination.isSetByUser)
        new StreamProfileReporter(new PrintWriter(new FileWriter(settings.YprofileDestination.value, true)))
      else ConsoleProfileReporter
      new RealProfiler(reporter, settings)
    }
}
case class GcEventData(pool:String, gcStartMillis:Long, gcEndMillis:Long)
//TODO separate the main thread wall clock time from the background threads times
case class ProfileCounters(wallClockTimeNanos : Long,
                           idleTimeNanos:Long, cpuTimeNanos: Long, userTimeNanos: Long,
                           allocatedBytes:Long, retainedHeapBytes:Long, gcTimeMillis:Long) {
  def +(that: ProfileCounters) = {
    ProfileCounters(
      wallClockTimeNanos = this.wallClockTimeNanos + that.wallClockTimeNanos,
      idleTimeNanos = this.idleTimeNanos + that.idleTimeNanos,
      cpuTimeNanos = this.cpuTimeNanos + that.cpuTimeNanos,
      userTimeNanos = this.userTimeNanos + that.userTimeNanos,
      allocatedBytes = this.allocatedBytes + that.allocatedBytes,
      retainedHeapBytes = this.retainedHeapBytes + that.retainedHeapBytes,
      gcTimeMillis = this.gcTimeMillis + that.gcTimeMillis)
  }

  def -(that: ProfileCounters) = {
    ProfileCounters(
      wallClockTimeNanos = this.wallClockTimeNanos - that.wallClockTimeNanos,
      idleTimeNanos = this.idleTimeNanos - that.idleTimeNanos,
      cpuTimeNanos = this.cpuTimeNanos - that.cpuTimeNanos,
      userTimeNanos = this.userTimeNanos - that.userTimeNanos,
      allocatedBytes = this.allocatedBytes - that.allocatedBytes,
      retainedHeapBytes = this.retainedHeapBytes - that.retainedHeapBytes,
      gcTimeMillis = this.gcTimeMillis - that.gcTimeMillis)
  }

  def updateHeap(heapDetails: ProfileCounters) = {
    copy(retainedHeapBytes = heapDetails.retainedHeapBytes)
  }

  private def toMillis(ns: Long) = ns / 1000000.0D

  private def toMegaBytes(bytes: Long) = bytes / 1000000.0D

  def wallClockTimeMillis = toMillis(wallClockTimeNanos)

  def idleTimeMillis = toMillis(idleTimeNanos)

  def cpuTimeMillis = toMillis(cpuTimeNanos)

  def userTimeMillis = toMillis(userTimeNanos)

  def allocatedMB = toMegaBytes(allocatedBytes)

  def retainedHeapMB = toMegaBytes(retainedHeapBytes)

}

sealed trait Profiler {
  /** Register an action. The action may be in the main thread or more typically in a background thread.
    * registration may occur in a different thread to execution
    */
  private[profile] def registerInPhase(action: InPhase): Unit

  /** Start to record an action. The action may be in the main thread or more typically in a background thread
    */
  private[profile] def beforeInPhase(action: InPhase): ProfileCounters

  /** Called after an action completes work
    */
  private[profile] def afterInPhase(action: InPhase, counterBefore: ProfileCounters, idleNs: Long): Unit

  def finished(): Unit

  def beforePhase(phase: Phase): ProfileCounters

  def afterPhase(phase: Phase, profileBefore: ProfileCounters): Unit

  protected val emptySnap = ProfileCounters(0, 0, 0, 0, 0, 0, 0)
}
private [profile] object NoOpProfiler extends Profiler {

  private[profile] override def registerInPhase(action: InPhase): Unit = ()
  /** Start to record an action. The action may be in the main thread or more typically in a background thread
    */
  private[profile] override def beforeInPhase(action: InPhase): ProfileCounters = emptySnap

  /** Called after an action completes work
    */
  private[profile] override def afterInPhase(action: InPhase, counterBefore: ProfileCounters, idleNs: Long): Unit = ()

  override def beforePhase(phase: Phase): ProfileCounters = emptySnap

  override def afterPhase(phase: Phase, profileBefore: ProfileCounters): Unit = ()

  override def finished(): Unit = ()
}
private [profile] object RealProfiler {
  import scala.collection.JavaConverters._
  val runtimeMx = ManagementFactory.getRuntimeMXBean
  val memoryMx = ManagementFactory.getMemoryMXBean
  val gcMx = ManagementFactory.getGarbageCollectorMXBeans.asScala.toList
  val classLoaderMx = ManagementFactory.getClassLoadingMXBean
  val compileMx = ManagementFactory.getCompilationMXBean
  val threadMx = ExtendedThreadMxBean.proxy
  if (threadMx.isThreadCpuTimeSupported) threadMx.setThreadCpuTimeEnabled(true)
  private val idGen = new AtomicInteger()
}

private [profile] class RealProfiler(reporter : ProfileReporter, val settings: Settings) extends Profiler with NotificationListener {
  def outDir = settings.outputDirs.getSingleOutput.getOrElse(settings.outputDirs.outputs.head._2.file).toString

  val id = RealProfiler.idGen.incrementAndGet()
  RealProfiler.gcMx foreach {
    case emitter: NotificationEmitter => emitter.addNotificationListener(this, null, null)
    case gc => println(s"Cant connect gcListener to ${gc.getClass}")
  }

  private val mainThread = Thread.currentThread()

  private def snap: ProfileCounters = {
    import RealProfiler._
    ProfileCounters(
      wallClockTimeNanos = System.nanoTime(),
      idleTimeNanos = 0L,
      cpuTimeNanos = threadMx.getCurrentThreadCpuTime,
      userTimeNanos = threadMx.getCurrentThreadUserTime,
      allocatedBytes = threadMx.getThreadAllocatedBytes(Thread.currentThread().getId),
      retainedHeapBytes = memoryMx.getHeapMemoryUsage.getUsed,
      gcTimeMillis = gcMx.foldLeft(0L) { case (sum, bean) => bean.getCollectionTime + sum }
    )
  }

  private def snapBackground(idleNs:Long): ProfileCounters = {
    import RealProfiler._
    ProfileCounters(
      wallClockTimeNanos = System.nanoTime(),
      idleTimeNanos = idleNs,
      cpuTimeNanos = threadMx.getCurrentThreadCpuTime,
      userTimeNanos = threadMx.getCurrentThreadUserTime,
      allocatedBytes = threadMx.getThreadAllocatedBytes(Thread.currentThread().getId),
      retainedHeapBytes = 0L,
      gcTimeMillis = 0L

    )
  }

  private def doGC: Unit = {
    System.gc()
    System.runFinalization()
  }

  reporter.header(this)

  override def finished(): Unit = {
    //we may miss a GC event if gc is occurring as we call this
    RealProfiler.gcMx foreach {
      case emitter: NotificationEmitter => emitter.removeNotificationListener(this)
      case gc =>
    }
    reporter.close(this)
  }


  override def handleNotification(notification: Notification, handback: scala.Any): Unit = {
    import java.lang.{Long => jLong}
    val data = notification.getUserData
    val seq = notification.getSequenceNumber
    val message = notification.getMessage
    val tpe = notification.getType
    val time= notification.getTimeStamp
    data match {
      case cd: CompositeData if tpe == "com.sun.management.gc.notification" =>
//        val name = cd.get("gcName").toString
//        val action = cd.get("gcAction").toString
//        val cause = cd.get("gcCause").toString
        val info = cd.get("gcInfo").asInstanceOf[CompositeData]
//        val duration = info.get("duration").asInstanceOf[jLong].longValue()
        val startTime = info.get("startTime").asInstanceOf[jLong].longValue()
        val endTime = info.get("endTime").asInstanceOf[jLong].longValue()
//        val threads = info.get("GcThreadCount").asInstanceOf[jLong].longValue()
        reporter.reportGc(new GcEventData("", startTime, endTime))
    }

  }

  var total = emptySnap

  override def afterPhase(phase: Phase, profileBefore: ProfileCounters): Unit = {
    assert(mainThread eq Thread.currentThread())
    val initialSnap = snap
    if (settings.YprofileExternalTool.containsPhase(phase)) {
      println("Profile hook stop")
      ExternalToolHook.after()
    }
    val finalSnap = if (settings.YprofileRunGcBetweenPhases.containsPhase(phase)) {
      doGC
      initialSnap.updateHeap(snap)
    } else initialSnap
    val mainThreadUsage = finalSnap - profileBefore
    threadInfo.synchronized {
      total += mainThreadUsage
      threadInfo(phase).afterPhase(mainThreadUsage)
    }
  }

  override def beforePhase(phase: Phase): ProfileCounters = {
    assert(mainThread eq Thread.currentThread())
    if (settings.YprofileRunGcBetweenPhases.containsPhase(phase))
      doGC
    if (settings.YprofileExternalTool.containsPhase(phase)) {
      println("Profile hook start")
      ExternalToolHook.before()
    }
    threadInfo(phase) = new ThreadInfo(phase)
    snap
  }

  private val threadInfo = mutable.Map[Phase, ThreadInfo]()

  /** called after an action completes work
    */

  override def registerInPhase(action: InPhase): Unit = threadInfo.synchronized{
      threadInfo.getOrElseUpdate(action.phase, new ThreadInfo(action.phase)).registerInPhase(action)
  }

  override def beforeInPhase(action: InPhase) = snapBackground(0L)

  override def afterInPhase(action: InPhase, profileBefore: ProfileCounters, idleNs: Long): Unit = threadInfo.synchronized {
    val inPhaseUsage = snapBackground(idleNs) - profileBefore
      threadInfo(action.phase).afterInPhase(action, inPhaseUsage)
  }

  class ThreadInfo(phase: Phase) {
    private var otherThreadsTotalUsage = emptySnap
    private var mainThreadUsage: ProfileCounters = _
    private var hasInPhase = false
    private val pending = mutable.Set[Int]()

    def registerInPhase(action: InPhase): Unit = {
      hasInPhase = true
      pending += action.id
    }

    def afterInPhase(action: InPhase, inPhaseUsage: ProfileCounters): Unit = {
      pending -= action.id
      if (mainThread != Thread.currentThread()) {
        otherThreadsTotalUsage += inPhaseUsage
        reporter.report(RealProfiler.this, phase, EventType.TASK, action.id, action.comment, inPhaseUsage)
        if ((pending isEmpty) && (mainThreadUsage ne null)) {
          reporter.report(RealProfiler.this, phase, EventType.TOTAL, -1, "--", mainThreadUsage + otherThreadsTotalUsage)
        }
      } else {
        reporter.report(RealProfiler.this, phase, EventType.TASK, action.id, action.comment, inPhaseUsage)
      }
    }

    def afterPhase(mainThreadUsage: ProfileCounters): Unit = {
      this.mainThreadUsage = mainThreadUsage
      val eventType = if (hasInPhase) EventType.MAIN else EventType.SINGLE
      reporter.report(RealProfiler.this, phase, eventType, -1, "--", mainThreadUsage)

      if (pending isEmpty) {
        reporter.report(RealProfiler.this, phase, EventType.TOTAL, -1, "--", mainThreadUsage + otherThreadsTotalUsage)
        total += otherThreadsTotalUsage
      } else {
        println("late reporting for " + phase)
      }
    }
  }
}

object EventType extends Enumeration {
  // only one report for a phase
  val SINGLE = Value("single")
  //main thread with other tasks
  val MAIN = Value("main")
  //other task ( background thread)
  val TASK = Value("task")
  //total for phase
  val TOTAL = Value("total")
  //total for compile
  val ALL = Value("all")
}
sealed trait ProfileReporter {
  def reportGc(data: GcEventData): Unit

  def report(profiler: RealProfiler, phase: Phase, eventType:EventType.Value, id:Int, desc:String, diff: ProfileCounters) : Unit

  def header(profiler: RealProfiler) :Unit
  def close(profiler: RealProfiler) :Unit
}

object ConsoleProfileReporter extends ProfileReporter {
  override def report(profiler: RealProfiler, phase: Phase, eventType:EventType.Value, id:Int, desc:String, diff: ProfileCounters): Unit =
    println(f"Profiler compile ${profiler.id} after phase ${phase.id}%2d:${phase.name}%20s ${eventType}%10s ${desc}%20s wallClockTime: ${diff.wallClockTimeMillis}%12.4fms, idleTime: ${diff.idleTimeMillis}%12.4fms, cpuTime ${diff.cpuTimeMillis}%12.4fms, userTime ${diff.userTimeMillis}%12.4fms, allocatedBytes ${diff.allocatedMB}%12.4fMB, retainedHeapBytes ${diff.retainedHeapMB}%12.4fMB, gcTime ${diff.gcTimeMillis}%6.0fms")

  override def close(profiler: RealProfiler): Unit = ()

  override def header(profiler: RealProfiler): Unit = {
    println(s"Profiler start (${profiler.id}) ${profiler.outDir}")
  }

  override def reportGc(data: GcEventData): Unit = {
    println(f"Profiler GC reported ${data.gcEndMillis - data.gcStartMillis}ms")
  }
}

class StreamProfileReporter(out:PrintWriter) extends ProfileReporter {
  override def header(profiler: RealProfiler): Unit = {
    out.println(s"info, ${profiler.id}, ${profiler.outDir}")
    out.println(s"header,id,phaseId,phaseName,type,id,comment,wallClockTimeMs,idleTimeMs,cpuTimeMs,userTimeMs,allocatedMB,retainedHeapMB,gcTimeMs")
  }
  override def report(profiler: RealProfiler, phase: Phase, eventType:EventType.Value, id:Int, desc:String, diff: ProfileCounters): Unit = {
    out.println(s"data,${profiler.id},${phase.id},${phase.name},${eventType},$id,$desc, ${diff.wallClockTimeMillis},${diff.idleTimeMillis},${diff.cpuTimeMillis},${diff.userTimeMillis},${diff.allocatedMB},${diff.retainedHeapMB},${diff.gcTimeMillis}")
  }

  override def reportGc(data: GcEventData): Unit = {
    out.println(s"GC,${data.gcStartMillis}, ${data.gcEndMillis}")
  }


  override def close(profiler: RealProfiler): Unit = {
    out.flush
    out.close
  }
}

