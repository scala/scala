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

import java.math.BigInteger
import java.nio.charset.StandardCharsets.UTF_8
import java.security.SecureRandom

import scala.annotation.tailrec
import scala.io.Codec
import scala.reflect.internal.util.OwnerOnlyChmod
import scala.reflect.internal.util.StringOps.splitWhere
import scala.tools.nsc.Properties.scalacDir
import scala.tools.nsc.io.File
import scala.util.control.NonFatal
import scala.util.Properties

trait HasCompileSocket {
  def compileSocket: CompileSocket

  // This is kind of a suboptimal way to identify error situations.
  val errorMarkers = Set("error:", "error found", "errors found", "bad option")
  def isErrorMessage(msg: String) = errorMarkers exists (msg contains _)

  def compileOnServer(sock: Socket, args: Seq[String]): Boolean = {
    var noErrors = true

    sock.applyReaderAndWriter { (in, out) =>
      out println (compileSocket getPassword sock.getPort())
      out println (args mkString "\u0000")

      @tailrec
      def loop(): Boolean = in.readLine() match {
        case null => noErrors
        case line =>
          if (isErrorMessage(line))
            noErrors = false

          // be consistent with scalac: everything goes to stderr
          compileSocket.warn(line)
          loop()
      }
      try loop()
      finally sock.close()
    }
  }
}

/** This class manages sockets for the fsc offline compiler.  */
class CompileSocket extends CompileOutputCommon {
  protected lazy val compileClient: StandardCompileClient = CompileClient
  def verbose = compileClient.verbose
  def verbose_=(v: Boolean) = compileClient.verbose = v
  /* Fixes the port where to start the server, 0 yields some free port */
  var fixPort = 0

  protected def cmdName = Properties.scalaCmd

  /** The vm part of the command to start a new scala compile server */
  protected val vmCommand = Properties.scalaHome match {
    case ""       => cmdName
    case dirname  =>
      val trial = File(dirname) / "bin" / cmdName
      if (trial.canRead) trial.path
      else cmdName
  }

  /** The class name of the scala compile server */
  protected val serverClass     = CompileServer.getClass.getName.init
  protected def serverClassArgs = (if (verbose) List("-v") else Nil) ::: (if (fixPort > 0) List("-p", fixPort.toString) else Nil)

  /* A directory holding port identification files */
  private lazy val portsDir = mkDaemonDir("fsc_port")

  /** The command which starts the compile server, given vm arguments.
    *
    *  @param vmArgs  the argument string to be passed to the java or scala command
    */
  private def serverCommand(vmArgs: Seq[String]): Seq[String] =
    Seq(vmCommand) ++ vmArgs ++ Seq(serverClass) ++ serverClassArgs filterNot (_ == "")

  /** Start a new server. */
  private def startNewServer(vmArgs: String): Unit = {
    val cmd = serverCommand((vmArgs split " ").toSeq)
    info(s"[Executing command: ${cmd.mkString(" ")}]")

    new java.lang.ProcessBuilder(cmd.toArray: _*).start()
  }

  /** The port identification file */
  def portFile(port: Int): File = portsDir / File(port.toString)

  /** Poll for a server port number; return -1 if none exists yet */
  private def pollPort(): Int = if (fixPort > 0) {
    if (portsDir.list.toList.exists(_.name == fixPort.toString)) fixPort else -1
  } else portsDir.list.toList match {
    case Nil      => -1
    case x :: _  => try x.name.toInt catch {
      case e: Exception => x.delete() ; throw e
    }
  }

  /** Get the port number to which a scala compile server is connected;
   *  If no server is running yet, then create one.
   */
  def getPort(vmArgs: String): Int = {
    val maxPolls = 300
    val sleepTime = 25L

    var attempts = 0
    var port = pollPort()

    if (port < 0) {
      info("No compile server running: starting one with args '" + vmArgs + "'")
      startNewServer(vmArgs)
    }
    while (port < 0 && attempts < maxPolls) {
      attempts += 1
      Thread.sleep(sleepTime)
      port = pollPort()
    }
    info("[Port number: " + port + "]")
    if (port < 0)
      fatal(s"Could not connect to compilation daemon after $attempts attempts. To run without it, use `-nocompdaemon` or `-nc`.")
    port
  }

  /** Set the port number to which a scala compile server is connected */
  def setPort(port: Int): Unit = {
    val file = portFile(port)
    // 128 bits of delicious randomness, suitable for printing with println over a socket,
    // and storage in a file -- see getPassword
    val secretDigits = new BigInteger(128, new SecureRandom()).toString.getBytes(UTF_8)

    try OwnerOnlyChmod.chmodFileAndWrite(file.jfile.toPath, secretDigits)
    catch chmodFailHandler(s"Cannot create file: ${file}")
  }

  /** Delete the port number to which a scala compile server was connected */
  def deletePort(port: Int) = portFile(port).delete()

  /** Get a socket connected to a daemon.  If create is true, then
    * create a new daemon if necessary.  Returns None if the connection
    * cannot be established.
    */
  def getOrCreateSocket(vmArgs: String, create: Boolean = true, fixedPort: Int = 0): Option[Socket] = {
    fixPort = fixedPort
    val maxMillis = 10L * 1000   // try for 10 seconds
    val retryDelay = 50L
    val maxAttempts = (maxMillis / retryDelay).toInt

    @tailrec
    def getsock(attempts: Int): Option[Socket] = attempts match {
      case 0 => warn("Unable to establish connection to compilation daemon") ; None
      case _ =>
        val port = if (create) getPort(vmArgs) else pollPort()
        if (port < 0) return None

        Socket.localhost(port).either match {
          case Right(socket)  =>
            info("[Connected to compilation daemon at port %d]" format port)
            Some(socket)
          case Left(err)      =>
            info(err.toString)
            info("[Connecting to compilation daemon at port %d failed; re-trying...]" format port)

            if (attempts % 2 == 0)
              deletePort(port)      // 50% chance to stop trying on this port

            Thread sleep retryDelay // delay before retrying
            getsock(attempts - 1)
        }
    }
    getsock(maxAttempts)
  }

  def getSocket(serverAdr: String): Option[Socket] = (
    for ((name, portStr) <- splitWhere(serverAdr, _ == ':', doDropIndex = true) ; port <- portStr.toIntOption) yield
      getSocket(name, port)
  ) getOrElse fatal("Malformed server address: %s; exiting" format serverAdr)

  def getSocket(hostName: String, port: Int): Option[Socket] = {
    val sock = Socket(hostName, port).opt
    if (sock.isEmpty) warn("Unable to establish connection to server %s:%d".format(hostName, port))
    sock
  }

  def getPassword(port: Int): String = {
    val ff  = portFile(port)
    val f   = ff.bufferedReader(Codec.UTF8)

    // allow some time for the server to start up
    def check = {
      Thread sleep 100
      ff.length
    }
    if ((Iterator continually check take 50 find (_ > 0)).isEmpty) {
      ff.delete()
      fatal("Unable to establish connection to server.")
    }
    val result = f.readLine()
    f.close()
    result
  }

  private def chmodFailHandler(msg: String): PartialFunction[Throwable, Unit] = {
    case NonFatal(e) =>
      if (verbose) e.printStackTrace()
      fatal(msg)
  }

  def mkDaemonDir(name: String) = {
    val dir = (scalacDir / name).createDirectory()

    if (dir.isDirectory && dir.canWrite) info(s"[Temp directory: $dir]")
    else fatal(s"Could not create compilation daemon directory $dir")

    try OwnerOnlyChmod.chmod(dir.jfile.toPath)
    catch chmodFailHandler(s"Failed to change permissions on $dir. The compilation daemon requires a secure directory; use -nc to disable the daemon.")
    dir
  }
}

object CompileSocket extends CompileSocket
