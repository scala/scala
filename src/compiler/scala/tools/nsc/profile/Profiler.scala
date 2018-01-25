package scala.tools.nsc.profile

import java.io.{FileWriter, PrintWriter}
import java.lang.management.ManagementFactory
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}

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

  private[profile] val emptySnap = ProfileSnap(0, "", 0, 0, 0, 0, 0, 0)
}
case class GcEventData(pool:String, reportTimeNs: Long, gcStartMillis:Long, gcEndMillis:Long, name:String, action:String, cause:String, threads:Long)

case class ProfileSnap(threadId: Long, threadName: String, snapTimeNanos : Long,
                         idleTimeNanos:Long, cpuTimeNanos: Long, userTimeNanos: Long,
                         allocatedBytes:Long, heapBytes:Long) {
  def updateHeap(heapBytes:Long) = {
    copy(heapBytes = heapBytes)
  }
}
case class ProfileRange(start: ProfileSnap, end:ProfileSnap, phase:Phase, purpose:String, taskCount:Int, thread:Thread) {
  def allocatedBytes = end.allocatedBytes - start.allocatedBytes

  def userNs = end.userTimeNanos - start.userTimeNanos

  def cpuNs = end.cpuTimeNanos - start.cpuTimeNanos

  def idleNs = end.idleTimeNanos - start.idleTimeNanos

  def runNs = end.snapTimeNanos - start.snapTimeNanos


  private def toMillis(ns: Long) = ns / 1000000.0D

  private def toMegaBytes(bytes: Long) = bytes / 1000000.0D


  def wallClockTimeMillis = toMillis(end.snapTimeNanos - start.snapTimeNanos)

  def idleTimeMillis = toMillis(end.idleTimeNanos - start.idleTimeNanos)

  def cpuTimeMillis = toMillis(end.cpuTimeNanos - start.cpuTimeNanos)

  def userTimeMillis = toMillis(end.userTimeNanos - start.userTimeNanos)

  def allocatedMB = toMegaBytes(end.allocatedBytes - start.allocatedBytes)

  def retainedHeapMB = toMegaBytes(end.heapBytes - start.heapBytes)
}

sealed trait Profiler {

  def finished(): Unit

  def beforePhase(phase: Phase): ProfileSnap

  def afterPhase(phase: Phase, profileBefore: ProfileSnap): Unit
}
private [profile] object NoOpProfiler extends Profiler {

  override def beforePhase(phase: Phase): ProfileSnap = Profiler.emptySnap

  override def afterPhase(phase: Phase, profileBefore: ProfileSnap): Unit = ()

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
  def completeBackground(threadRange: ProfileRange): Unit = {
    reporter.reportBackground(this, threadRange)
  }

  def outDir = settings.outputDirs.getSingleOutput.getOrElse(settings.outputDirs.outputs.head._2.file).toString

  val id = RealProfiler.idGen.incrementAndGet()
  RealProfiler.gcMx foreach {
    case emitter: NotificationEmitter => emitter.addNotificationListener(this, null, null)
    case gc => println(s"Cant connect gcListener to ${gc.getClass}")
  }

  private val mainThread = Thread.currentThread()

  private[profile] def snapThread( idleTimeNanos:Long): ProfileSnap = {
    import RealProfiler._
    val current = Thread.currentThread()

    ProfileSnap(
      threadId = current.getId,
      threadName = current.getName,
      snapTimeNanos = System.nanoTime(),
      idleTimeNanos = idleTimeNanos,
      cpuTimeNanos = threadMx.getCurrentThreadCpuTime,
      userTimeNanos = threadMx.getCurrentThreadUserTime,
      allocatedBytes = threadMx.getThreadAllocatedBytes(Thread.currentThread().getId),
      heapBytes = readHeapUsage()
    )
  }
  private def readHeapUsage() = RealProfiler.memoryMx.getHeapMemoryUsage.getUsed

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
    import java.lang.{Integer => jInt}
    val reportNs = System.nanoTime()
    val data = notification.getUserData
    val seq = notification.getSequenceNumber
    val message = notification.getMessage
    val tpe = notification.getType
    val time= notification.getTimeStamp
    data match {
      case cd: CompositeData if tpe == "com.sun.management.gc.notification" =>
        val name = cd.get("gcName").toString
        val action = cd.get("gcAction").toString
        val cause = cd.get("gcCause").toString
        val info = cd.get("gcInfo").asInstanceOf[CompositeData]
        val duration = info.get("duration").asInstanceOf[jLong].longValue()
        val startTime = info.get("startTime").asInstanceOf[jLong].longValue()
        val endTime = info.get("endTime").asInstanceOf[jLong].longValue()
        val threads = info.get("GcThreadCount").asInstanceOf[jInt].longValue()
        reporter.reportGc(GcEventData("", reportNs, startTime, endTime, name, action, cause, threads))
    }
  }

  override def afterPhase(phase: Phase, snapBefore: ProfileSnap): Unit = {
    assert(mainThread eq Thread.currentThread())
    val initialSnap = snapThread(0)
    if (settings.YprofileExternalTool.containsPhase(phase)) {
      println("Profile hook stop")
      ExternalToolHook.after()
    }
    val finalSnap = if (settings.YprofileRunGcBetweenPhases.containsPhase(phase)) {
      doGC
      initialSnap.updateHeap(readHeapUsage())
    } else initialSnap

    reporter.reportForeground(this, ProfileRange(snapBefore, finalSnap, phase, "", 0, Thread.currentThread))
  }

  override def beforePhase(phase: Phase): ProfileSnap = {
    assert(mainThread eq Thread.currentThread())
    if (settings.YprofileRunGcBetweenPhases.containsPhase(phase))
      doGC
    if (settings.YprofileExternalTool.containsPhase(phase)) {
      println("Profile hook start")
      ExternalToolHook.before()
    }
    snapThread(0)
  }

}

object EventType extends Enumeration {
  type value = Value
  //main thread with other tasks
  val MAIN = Value("main")
  //other task ( background thread)
  val BACKGROUND = Value("background")
  //total for compile
  val GC = Value("GC")
}

sealed trait ProfileReporter {
  def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit
  def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit

  def reportGc(data: GcEventData): Unit

  def header(profiler: RealProfiler) :Unit
  def close(profiler: RealProfiler) :Unit
}

object ConsoleProfileReporter extends ProfileReporter {


  override def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit =
  // TODO
    ???
  override def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit =
  // TODO
    ???

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
    out.println(s"info, ${profiler.id}, version, 2, output, ${profiler.outDir}")
    out.println(s"header(main/background),startNs,endNs,runId,phaseId,phaseName,purpose,task-count,threadId,threadName,runNs,idleNs,cpuTimeNs,userTimeNs,allocatedByte,heapSize")
    out.println(s"header(GC),startNs,endNs,startMs,endMs,name,action,cause,threads")
  }

  override def reportBackground(profiler: RealProfiler, threadRange: ProfileRange): Unit = {
    reportCommon(EventType.BACKGROUND, profiler, threadRange)
  }
  override def reportForeground(profiler: RealProfiler, threadRange: ProfileRange): Unit = {
    reportCommon(EventType.MAIN, profiler, threadRange)
  }
  private def reportCommon(tpe:EventType.value, profiler: RealProfiler, threadRange: ProfileRange): Unit = {
    out.println(s"$tpe,${threadRange.start.snapTimeNanos},${threadRange.end.snapTimeNanos},${profiler.id},${threadRange.phase.id},${threadRange.phase.name},${threadRange.purpose},${threadRange.taskCount},${threadRange.thread.getId},${threadRange.thread.getName},${threadRange.runNs},${threadRange.idleNs},${threadRange.cpuNs},${threadRange.userNs},${threadRange.allocatedBytes},${threadRange.end.heapBytes} ")
  }

  override def reportGc(data: GcEventData): Unit = {
    val duration = TimeUnit.MILLISECONDS.toNanos(data.gcEndMillis - data.gcStartMillis + 1)
    val start = data.reportTimeNs - duration
    out.println(s"${EventType.GC},$start,${data.reportTimeNs},${data.gcStartMillis}, ${data.gcEndMillis},${data.name},${data.action},${data.cause},${data.threads}")
  }


  override def close(profiler: RealProfiler): Unit = {
    out.flush
    out.close
  }
}

