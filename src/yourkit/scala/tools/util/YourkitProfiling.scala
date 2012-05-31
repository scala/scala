package scala.tools
package util

import com.yourkit.api._
import com.yourkit.runtime._
import nsc.io._

class YourkitProfiling extends Profiling {
  @volatile private var active = false
  @volatile private var freq: Option[Int] = None
  lazy val controller = new Controller

  def defaultFreq = 100
  def allocationFreq = freq
  def setAllocationFreq(x: Int) = freq = if (x <= 0) None else Some(x)

  def startRecordingAllocations() = {
    controller.startAllocationRecording(true, freq getOrElse defaultFreq, false, 0)
  }
  def stopRecordingAllocations() = {
    controller.stopAllocationRecording()
  }

  def startProfiling(): Unit = {
    if (isActive)
      return

    active = true
    daemonize {
      try {
        controller.startCPUProfiling(ProfilingModes.CPU_SAMPLING, Controller.DEFAULT_FILTERS)
        if (freq.isDefined)
          startRecordingAllocations()
      }
      catch {
        case _: PresentableException  => () // if it's already running, no big deal
      }
    }
  }

  def captureSnapshot() = {
    daemonize(controller.captureSnapshot(ProfilingModes.SNAPSHOT_WITH_HEAP))
  }

  def stopProfiling() = {
    try {
      if (freq.isDefined)
        stopRecordingAllocations()

      controller.stopCPUProfiling()
    }
    catch {
      case _: PresentableException  => () // if it's already running, no big deal
    }
    finally active = false
  }

  def advanceGeneration(desc: String) {
    controller.advanceGeneration(desc)
  }

  def isActive = active
}
