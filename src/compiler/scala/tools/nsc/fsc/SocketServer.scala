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

package scala.tools.nsc.fsc

import scala.tools.util.SystemExit
import java.io.{BufferedReader, PrintStream, PrintWriter}
import java.net.{ServerSocket, SocketException, SocketTimeoutException}
import scala.annotation.tailrec

trait CompileOutputCommon {
  def verbose: Boolean

  def info(msg: String)  = if (verbose) echo(msg)
  def echo(msg: String)  = printlnFlush(msg, Console.out)
  def warn(msg: String)  = printlnFlush(msg, Console.err)
  def fatal(msg: String) = { warn(msg) ; throw SystemExit(1) }

  private def printlnFlush(msg: String, out: PrintStream) = {
    out.println(msg)
    out.flush()
  }
}

/** The abstract class SocketServer implements the server
 *  communication for the fast Scala compiler.
 *
 *  @author  Martin Odersky
 */
abstract class SocketServer(fixPort: Int = 0) extends CompileOutputCommon {
  def shutdown: Boolean
  def session(): Unit
  def timeout(): Unit = ()  // called after a timeout is detected for subclasses to cleanup
  // a hook for subclasses
  protected def createServerSocket(): ServerSocket = new ServerSocket(fixPort)

  var in: BufferedReader = _
  var out: PrintWriter   = _
  val BufferSize         = 10240
  lazy val serverSocket  = createServerSocket()
  lazy val port          = serverSocket.getLocalPort()

  // Default to 30 minute idle timeout, settable with -max-idle
  protected var idleMinutes = 30
  private var savedTimeout = 0
  private val acceptBox = new Socket.Box(() => {
    // update the timeout if it has changed
    if (savedTimeout != idleMinutes) {
      savedTimeout = idleMinutes
      setTimeoutOnSocket(savedTimeout)
    }
    new Socket(serverSocket.accept())
  })
  private def setTimeoutOnSocket(mins: Int) = {
    try {
      serverSocket setSoTimeout (mins * 60 * 1000)
      info("Set socket timeout to " + mins + " minutes.")
      true
    }
    catch {
      case ex: SocketException =>
        warn("Failed to set socket timeout: " + ex)
        false
    }
  }

  def doSession(clientSocket: Socket) = {
    clientSocket.applyReaderAndWriter { (in, out) =>
      this.in    = in
      this.out   = out
      val bufout = clientSocket.bufferedOutput(BufferSize)

      try scala.Console.withOut(bufout)(session())
      finally bufout.close()
    }
  }

  def run(): Unit = {
    info("Starting SocketServer run() loop.")

    @tailrec
    def loop(): Unit = {
      acceptBox.either match {
        case Right(clientSocket) =>
          try doSession(clientSocket)
          finally clientSocket.close()
        case Left(_: SocketTimeoutException) =>
          warn("Idle timeout exceeded on port %d; exiting" format port)
          timeout()
          return
        case _ =>
          warn(s"Accept on port $port failed")
      }
      if (!shutdown)
        loop()
    }
    try loop()
    catch { case ex: SocketException => fatal("Compile server caught fatal exception: " + ex) }
    finally serverSocket.close()
  }
}
