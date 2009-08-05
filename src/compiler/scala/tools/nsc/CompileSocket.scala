/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.lang.{Thread, System, Runtime}
import java.lang.NumberFormatException
import java.io.{File, IOException, PrintWriter, FileOutputStream}
import java.io.{BufferedReader, FileReader}
import java.util.regex.Pattern
import java.net._

/** This class manages sockets for the fsc offline compiler.  */
class CompileSocket {
  protected def compileClient: StandardCompileClient = CompileClient //todo: lazy val

  /** The prefix of the port identification file, which is followed
   *  by the port number.
   */
  protected def dirName = "scalac-compile-server-port" //todo: lazy val

  protected def cmdName = Properties.cmdName //todo: lazy val

  /** The vm part of the command to start a new scala compile server */
  protected val vmCommand =
    Properties.scalaHome match {
      case null =>
        cmdName
      case dirname =>
        val trial = new File(new File(dirname, "bin"), cmdName)
        if (trial.canRead)
          trial.getPath
        else
          cmdName
    }

  /** The class name of the scala compile server */
  protected val serverClass = "scala.tools.nsc.CompileServer"

  /** A regular expression for checking compiler output for errors */
  val errorRegex = ".*(errors? found|don't know|bad option).*"

  /** A Pattern object for checking compiler output for errors */
  val errorPattern = Pattern.compile(errorRegex)

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
    val f     = (scala.io.File(Properties.tmpDir) / "scala-devel" / udir).file
    f.mkdirs()

    if (f.isDirectory && f.canWrite) {
      info("[Temp directory: " + f + "]")
      f
    }
    else fatal("Could not find a directory for temporary files")
  }

  /* A directory holding port identification files */
  val portsDir =  new File(tmpDir, dirName)
  portsDir.mkdirs

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
  def portFile(port: Int) = new File(portsDir, port.toString())

  /** Poll for a server port number; return -1 if none exists yet */
  private def pollPort(): Int = {
    val hits = portsDir.listFiles()
    if (hits.length == 0) -1
    else
      try {
        for (i <- 1 until hits.length) hits(i).delete()
        hits(0).getName.toInt
      } catch {
        case ex: NumberFormatException =>
          fatal(ex.toString() +
                "\nbad file in temp directory: " +
                hits(0).getAbsolutePath() +
                "\nplease remove the file and try again")
      }
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
    try {
      val f = new PrintWriter(new FileOutputStream(portFile(port)))
      f.println(new java.security.SecureRandom().nextInt.toString)
      f.close()
    } catch {
      case ex: /*FileNotFound+Security*/Exception =>
        fatal("Cannot create file: " +
              portFile(port).getAbsolutePath())
    }
  }

  /** Delete the port number to which a scala compile server was connected */
  def deletePort(port: Int) { portFile(port).delete() }

  /** Get a socket connected to a daemon.  If create is true, then
    * create a new daemon if necessary.  Returns null if the connection
    * cannot be established.
    */
  def getOrCreateSocket(vmArgs: String, create: Boolean): Socket = {
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

  /** Same as getOrCreateSocket(vmArgs, true). */
  def getOrCreateSocket(vmArgs: String): Socket =
    getOrCreateSocket(vmArgs, true)

  def getSocket(serverAdr: String): Socket = {
    val cpos = serverAdr indexOf ':'
    if (cpos < 0)
      fatal("Malformed server address: " + serverAdr + "; exiting")
    else {
      val hostName = serverAdr.substring(0, cpos)
      val port = try {
        serverAdr.substring(cpos+1).toInt
      } catch {
        case ex: Throwable =>
          fatal("Malformed server address: " + serverAdr + "; exiting")
      }
      getSocket(hostName, port)
    }
  }

  def getSocket(hostName: String, port: Int): Socket =
    try {
      new Socket(hostName, port)
    } catch {
      case e: /*IO+Security*/Exception =>
        fatal("Unable to establish connection to server " +
              hostName + ":" + port + "; exiting")
    }

  def getPassword(port: Int): String = {
    val ff = portFile(port)
    val f = new BufferedReader(new FileReader(ff))
    // allow some time for the server to start up
    var retry = 50
    while (ff.length() == 0 && retry > 0) {
      Thread.sleep(100)
      retry -= 1
    }
    if (ff.length() == 0) {
      ff.delete()
      fatal("Unable to establish connection to server.")
    }
    val result = f.readLine()
    f.close()
    result
  }
}


object CompileSocket extends CompileSocket
