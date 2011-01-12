/*
 *  @author Stephane Micheloud
 */

import java.io.{BufferedReader, InputStreamReader}

import scala.compat.Platform.currentTime
import scala.remoting.Debug, Debug._

trait ServerConsole extends Thread {
  private val startTime = currentTime
  actors.Debug.level = // e.g. 3 // info+warning+error
    try { System.getProperty("scala.actors.logLevel", "0").toInt }
    catch { case e => 0 }

  start()

  val serverClassLoader = {
    import java.rmi.server.RMIClassLoader
    val codebase = System.getProperty("java.rmi.server.codebase")
    info("[ServerConsole] codebase="+codebase)
    RMIClassLoader getClassLoader codebase
  }

  private var isTerminated = false

  def terminate() { isTerminated = false }

  def isRunning = !isTerminated

  override def run() {
    val in = new BufferedReader(new InputStreamReader(System.in))
    var quit = false
    while (!quit) {
      val args = getArgs(in)
      if (args contains "quit")
        quit = true
      if (args contains "cls") {
        println(ERASE_SCREEN)
        println(CURSOR_HOME)
      }
      if (args contains "warning")
        Debug.level = Level.WARNING
      if (args contains "info")
        Debug.level = Level.INFO
      if (args contains "silent")
        Debug.level = Level.SILENT
    }
    terminate()
    println("Server exited ("+mkTimeString(currentTime - startTime)+")")
    sys.exit(0)
  }

  protected def trace(msg: String) {
    Debug.info("[ServerConsole.trace] "+msg)
  }

  private def getArgs(in: BufferedReader): List[String] = {
    val input = try { in.readLine() } catch { case _ => null }
    if (input != null) (input.trim split "\\s+").toList else Nil
  }

  private def mkTimeString(time: Long): String = {
    def twoDigits(i: Long) = (if (i < 10) "0" else "")+i
    val sec = time / 1000
    val min = sec / 60
    val h = min / 60
    twoDigits(h) +":"+
    twoDigits(min - h * 60)+":"+
    twoDigits(sec - min * 60)
  }

  private val ERASE_SCREEN = "\033[2J"
  private val CURSOR_HOME  = "\033[H"
}
