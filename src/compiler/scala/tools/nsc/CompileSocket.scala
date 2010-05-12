/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.{ IOException, FileNotFoundException, PrintWriter, FileOutputStream }
import java.io.{ BufferedReader, FileReader }
import java.util.regex.Pattern
import java.net._
import java.security.SecureRandom

import io.{ File, Path, Process, Socket }
import scala.util.control.Exception.catching
import scala.tools.util.StringOps.splitWhere

/** This class manages sockets for the fsc offline compiler.  */
class CompileSocket {
  protected def compileClient: StandardCompileClient = CompileClient //todo: lazy val

  /** The prefix of the port identification file, which is followed
   *  by the port number.
   */
  protected lazy val dirName = "scalac-compile-server-port"
  protected lazy val cmdName = Properties.scalaCmd

  /** The vm part of the command to start a new scala compile server */
  protected val vmCommand = Properties.scalaHome match {
    case ""       => cmdName
    case dirname  =>
      val trial = File(dirname) / "bin" / cmdName
      if (trial.canRead) trial.path
      else cmdName
  }

  /** The class name of the scala compile server */
  protected val serverClass = "scala.tools.nsc.CompileServer"

  /** A regular expression for checking compiler output for errors */
  val errorRegex = ".*(errors? found|don't know|bad option).*"

  /** A Pattern object for checking compiler output for errors */
  val errorPattern = Pattern compile errorRegex

  protected def error(msg: String) = System.err.println(msg)

  protected def fatal(msg: String) = {
    error(msg)
    throw new Exception("fsc failure")
  }

  protected def info(msg: String) =
    if (compileClient.verbose) System.out.println(msg)

  /** A temporary directory to use */
  val tmpDir = {
    val udir  = Option(Properties.userName) getOrElse "shared"
    val f     = (Path(Properties.tmpDir) / "scala-devel" / udir).createDirectory()

    if (f.isDirectory && f.canWrite) {
      info("[Temp directory: " + f + "]")
      f
    }
    else fatal("Could not find a directory for temporary files")
  }

  /* A directory holding port identification files */
  val portsDir = (tmpDir / dirName).createDirectory()

  /** Maximum number of polls for an available port */
  private val MaxAttempts = 100

  /** Time (in ms) to sleep between two polls */
  private val sleepTime = 20

  /** The command which starts the compile server, given vm arguments.
    *
    *  @param vmArgs  the argument string to be passed to the java or scala command
    */
  private def serverCommand(vmArgs: Seq[String]): Seq[String] =
    Seq(vmCommand) ++ vmArgs ++ Seq(serverClass) filterNot (_ == "")

  /** Start a new server; returns true iff it succeeds */
  private def startNewServer(vmArgs: String) {
    val cmd = serverCommand(vmArgs split " " toSeq)
    info("[Executed command: %s]" format cmd)
    try Process exec cmd catch {
      case ex: IOException => fatal("Cannot start compilation daemon.\ntried command: %s" format cmd)
    }
  }

  /** The port identification file */
  def portFile(port: Int) = portsDir / File(port.toString)

  /** Poll for a server port number; return -1 if none exists yet */
  private def pollPort(): Int = portsDir.list match {
    case it if !it.hasNext  => -1
    case it                 =>
      val ret = it.next.name.toInt
      it foreach (_.delete())
      ret
  }

  /** Get the port number to which a scala compile server is connected;
   *  If no server is running yet, then create one.
   */
  def getPort(vmArgs: String): Int = {
    var attempts = 0
    var port = pollPort()

    if (port < 0)
      startNewServer(vmArgs)

    while (port < 0 && attempts < MaxAttempts) {
      attempts += 1
      Thread.sleep(sleepTime)
      port = pollPort()
    }
    info("[Port number: " + port + "]")
    if (port < 0)
      fatal("Could not connect to compilation daemon.")
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
  def getOrCreateSocket(vmArgs: String, create: Boolean = true): Option[Socket] = {
    // try for 5 seconds
    val retryDelay = 100
    val maxAttempts = (5 * 1000) / retryDelay

    def getsock(attempts: Int): Option[Socket] = attempts match {
      case 0    => error("Unable to establish connection to compilation daemon") ; None
      case num  =>
        val port = if (create) getPort(vmArgs) else pollPort()
        if (port < 0) return None

        Socket(InetAddress.getLocalHost(), port).either match {
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

  def getSocket(serverAdr: String): Socket = (
    for ((name, portStr) <- splitWhere(serverAdr, _ == ':', true) ; port <- parseInt(portStr)) yield
      getSocket(name, port)
  ) getOrElse fatal("Malformed server address: %s; exiting" format serverAdr)

  def getSocket(hostName: String, port: Int): Socket =
    Socket(hostName, port).opt getOrElse fatal("Unable to establish connection to server %s:%d; exiting".format(hostName, port))

  def getPassword(port: Int): String = {
    val ff  = portFile(port)
    val f   = ff.bufferedReader()

    // allow some time for the server to start up
    def check = {
      Thread sleep 100
      ff.length
    }
    if (Iterator continually check take 50 find (_ > 0) isEmpty) {
      ff.delete()
      fatal("Unable to establish connection to server.")
    }
    val result = f.readLine()
    f.close()
    result
  }
}


object CompileSocket extends CompileSocket
