/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.FileNotFoundException
import java.security.SecureRandom
import io.{ File, Path, Socket }
import scala.tools.util.CompileOutputCommon
import scala.reflect.internal.util.StringOps.splitWhere
import scala.sys.process._

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

  /* Fixes the port where to start the server, 0 yields some free port */
  var fixPort = 0

  /** The prefix of the port identification file, which is followed
   *  by the port number.
   */
  protected lazy val dirName = "scalac-compile-server-port"
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
  protected val serverClass     = "scala.tools.nsc.CompileServer"
  protected def serverClassArgs = (if (verbose) List("-v") else Nil) ::: (if (fixPort > 0) List("-p", fixPort.toString) else Nil)

  /** A temporary directory to use */
  val tmpDir = {
    val udir  = Option(Properties.userName) getOrElse "shared"
    val f     = (Path(Properties.tmpDir) / ("scala-devel" + udir)).createDirectory()

    if (f.isDirectory && f.canWrite) {
      info("[Temp directory: " + f + "]")
      f
    }
    else fatal("Could not find a directory for temporary files")
  }

  /* A directory holding port identification files */
  val portsDir = (tmpDir / dirName).createDirectory()

  /** The command which starts the compile server, given vm arguments.
    *
    *  @param vmArgs  the argument string to be passed to the java or scala command
    */
  private def serverCommand(vmArgs: Seq[String]): Seq[String] =
    Seq(vmCommand) ++ vmArgs ++ Seq(serverClass) ++ serverClassArgs filterNot (_ == "")

  /** Start a new server. */
  private def startNewServer(vmArgs: String) = {
    val cmd = serverCommand((vmArgs split " ").toSeq)
    info("[Executing command: %s]" format cmd.mkString(" "))

    // Hiding inadequate daemonized implementation from public API for now
    Process(cmd) match {
      case x: ProcessBuilder.AbstractBuilder => x.daemonized().run()
      case x                                 => x.run()
    }
  }

  /** The port identification file */
  def portFile(port: Int) = portsDir / File(port.toString)

  /** Poll for a server port number; return -1 if none exists yet */
  private def pollPort(): Int = if (fixPort > 0) {
  	if (portsDir.list.toList.exists(_.name == fixPort.toString)) fixPort else -1
  } else portsDir.list.toList match {
    case Nil      => -1
    case x :: xs  => try x.name.toInt catch {
    	case e: Exception => x.delete()
    	throw e
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
      fatal("Could not connect to compilation daemon after " + attempts + " attempts.")
    port
  }

  /** Set the port number to which a scala compile server is connected */
  def setPort(port: Int) {
    val file    = portFile(port)
    val secret  = new SecureRandom().nextInt.toString

    try file writeAll secret catch {
      case e @ (_: FileNotFoundException | _: SecurityException) =>
        fatal("Cannot create file: %s".format(file.path))
    }
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

    def getsock(attempts: Int): Option[Socket] = attempts match {
      case 0    => warn("Unable to establish connection to compilation daemon") ; None
      case num  =>
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

  // XXX way past time for this to be central
  def parseInt(x: String): Option[Int] =
    try   { Some(x.toInt) }
    catch { case _: NumberFormatException => None }

  def getSocket(serverAdr: String): Option[Socket] = (
    for ((name, portStr) <- splitWhere(serverAdr, _ == ':', doDropIndex = true) ; port <- parseInt(portStr)) yield
      getSocket(name, port)
  ) getOrElse fatal("Malformed server address: %s; exiting" format serverAdr)

  def getSocket(hostName: String, port: Int): Option[Socket] = {
    val sock = Socket(hostName, port).opt
    if (sock.isEmpty) warn("Unable to establish connection to server %s:%d".format(hostName, port))
    sock
  }

  def getPassword(port: Int): String = {
    val ff  = portFile(port)
    val f   = ff.bufferedReader()

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
}


object CompileSocket extends CompileSocket {
}
