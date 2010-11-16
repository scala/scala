package scala.tools
package util

import com.yourkit.api._
import nsc.io._

class YourkitProfiling extends Profiling {
  @volatile private var active = false
  private var recordAllocation = false
  lazy val controller = new Controller

  def startProfiling(): Unit = {
    if (isActive)
      return

    active = true
    daemonize(true) {
      controller.startCPUProfiling(ProfilingModes.CPU_SAMPLING, Controller.DEFAULT_FILTERS)
      if (recordAllocation)
        controller.startAllocationRecording(true, 100, false, 0)
    }
  }

  def captureSnapshot() =
    daemonize(false)(controller.captureSnapshot(ProfilingModes.SNAPSHOT_WITH_HEAP))

  def stopProfiling() = {
    if (recordAllocation)
      controller.stopAllocationRecording()

    controller.stopCPUProfiling()
    active = false
  }

  def isActive = active
}