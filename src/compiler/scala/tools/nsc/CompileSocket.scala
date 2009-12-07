/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.{ IOException, FileNotFoundException, PrintWriter, FileOutputStream }
import java.io.{ BufferedReader, FileReader }
import java.util.regex.Pattern
import java.net._
import java.security.SecureRandom

import io.{ File, Path }
import scala.util.control.Exception.catching

// class CompileChannel { }

/** This class manages sockets for the fsc offline compiler.  */
class CompileSocket {
  protected def compileClient: StandardCompileClient = CompileClient //todo: lazy val

  /** The prefix of the port identification file, which is followed
   *  by the port number.
   */
  protected def dirName = "scalac-compile-server-port" //todo: lazy val

  protected def cmdName = Properties.cmdName //todo: lazy val

  /** The vm part of the command to start a new scala compile server */
  protected val vmCommand = Properties.scalaHome match {
    case null     => cmdName
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
    *                 the string must be either empty or start with a ' '.
    */
  private def serverCommand(vmArgs: String): String =
    vmCommand + vmArgs + " " + serverClass

  /** Start a new server; returns true iff it succeeds */
  private def startNewServer(vmArgs: String) {
    val cmd = serverCommand(vmArgs)
    info("[Executed command: " + cmd + "]")
    try {
      Runtime.getRuntime().exec(cmd)
//      val exitVal = proc.waitFor()
//      info("[Exit value: " + exitVal + "]")
    } catch {
      case ex: IOException =>
        fatal("Cannot start compilation daemon." +
              "\ntried command: " + cmd)
    }
  }

  /** The port identification file */
  def portFile(port: Int) = portsDir / File(port.toString)

  /** Poll for a server port number; return -1 if none exists yet */
  private def pollPort(): Int =
    portsDir.list.toList match {
      case Nil      => -1
      case p :: xs  =>
        xs forall (_.delete())
        p.name.toInt
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

    try file writeAll List(secret) catch {
      case e @ (_: FileNotFoundException | _: SecurityException) =>
        fatal("Cannot create file: %s".format(file.path))
    }
  }

  /** Delete the port number to which a scala compile server was connected */
  def deletePort(port: Int) = portFile(port).delete()

  /** Get a socket connected to a daemon.  If create is true, then
    * create a new daemon if necessary.  Returns null if the connection
    * cannot be established.
    */
  def getOrCreateSocket(vmArgs: String, create: Boolean = true): Socket = {
    val nAttempts = 49  // try for about 5 seconds
    def getsock(attempts: Int): Socket =
      if (attempts == 0) {
        error("Unable to establish connection to compilation daemon")
        null
      } else {
        val port = if(create) getPort(vmArgs) else pollPort()
        if(port < 0) return null
        val hostAdr = InetAddress.getLocalHost()
        try {
          val result = new Socket(hostAdr, port)
          info("[Connected to compilation daemon at port " + port + "]")
          result
        } catch {
          case e: /*IO+Security*/Exception =>
            info(e.toString)
            info("[Connecting to compilation daemon at port "  +
                 port + " failed; re-trying...]")

            if (attempts % 2 == 0)
              portFile(port).delete // 50% chance to stop trying on this port

            Thread.sleep(100) // delay before retrying

            getsock(attempts - 1)
        }
      }
    getsock(nAttempts)
  }

  // XXX way past time for this to be central
  def parseInt(x: String): Option[Int] =
    try   { Some(x.toInt) }
    catch { case _: NumberFormatException => None }

  def getSocket(serverAdr: String): Socket = {
    def fail = fatal("Malformed server address: %s; exiting" format serverAdr)
    (serverAdr indexOf ':') match {
      case -1   => fail
      case cpos =>
        val hostName: String = serverAdr take cpos
        parseInt(serverAdr drop (cpos + 1)) match {
          case Some(port) => getSocket(hostName, port)
          case _          => fail
        }
    }
  }

  def getSocket(hostName: String, port: Int): Socket =
    try new Socket(hostName, port) catch {
      case e @ (_: IOException | _: SecurityException) =>
        fatal("Unable to establish connection to server %s:%d; exiting".format(hostName, port))
    }

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
