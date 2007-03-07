/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
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

object CompileSocket {

  /** The prefix of the port identification file, which is followed
   *  by the port number.
   */
  private val dirName = "scalac-compile-server-port"

  /** The vm-part of the command to start a new scala compile server */
  private val vmCommand =
    Properties.scalaHome match {
      case null =>
        Properties.cmdName
      case dirname =>
        val trial = new File(new File(dirname, "bin"), Properties.cmdName)
        if (trial.canRead)
          trial.getPath
        else
          Properties.cmdName
    }

  /** The class name of the scala compile server */
  private val serverClass = "scala.tools.nsc.CompileServer"

  /** A regular expression for checking compiler output for errors */
  val errorRegex = ".*errors? found.*"

  /** A Pattern object for checking compiler output for errors */
  val errorPattern = Pattern.compile(errorRegex)

  private def error(msg: String) = System.err.println(msg)

  private def fatal(msg: String) = {
    error(msg)
    exit(1)
  }

  private def info(msg: String) =
    if (CompileClient.verbose) System.out.println(msg)

  /** A temporary directory to use */
  val tmpDir = {
    val totry = List(
        ("scala.home", List("var", "scala-devel")),
        ("user.home", List("tmp")),
        ("java.io.tmpdir", Nil))

    /** Expand a property-extensions pair into a complete File object */
    def expand(trial: (String, List[String])): Option[File] = {
      val (topdirProp, extensions) = trial
      val topdir = System.getProperty(topdirProp)
      if (topdir eq null)
        return None

      val fulldir =
        extensions.foldLeft[File](new File(topdir))(
            (dir,ext) => new File(dir, ext))

      Some(fulldir)
    }

    /** Test if file f is a writable directory */
    def isDirWritable(f: File): Boolean =
      f.isDirectory && f.canWrite

    val potentials =
      for {
        val trial <- totry
        val expanded = expand(trial)
        !expanded.isEmpty
        isDirWritable(expanded.get)
      }
      yield expanded.get

    if (potentials.isEmpty)
      fatal("Could not find a directory for temporary files")
    else {
      val d = potentials.head
      d.mkdirs
      info("[Temp directory: " + d + "]")
      d
    }
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
  private def startNewServer(vmArgs: String): unit = {
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
  def portFile(port: int) = new File(portsDir, port.toString())

  /** Poll for a server port number; return -1 if none exists yet */
  private def pollPort(): int = {
    val hits = portsDir.listFiles()
    if (hits.length == 0) -1
    else
      try {
        for (val i <- 1 until hits.length) hits(i).delete()
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
  def getPort(vmArgs: String): int = {
    var attempts = 0
    var port = pollPort()

    if (port < 0)
      startNewServer(vmArgs)
    while (port < 0 && attempts < MaxAttempts) {
      attempts = attempts + 1
      Thread.sleep(sleepTime)
      port = pollPort()
    }
    info("[Port number: " + port + "]")
    if (port < 0)
      fatal("Could not connect to compilation daemon.")
    port
  }

  /** Set the port number to which a scala compile server is connected */
  def setPort(port: int): unit =
    try {
      val f = new PrintWriter(new FileOutputStream(portFile(port)))
      f.println(new java.util.Random().nextInt.toString)
      f.close()
    } catch {
      case ex: /*FileNotFound+Security*/Exception =>
        fatal("Cannot create file: " +
              portFile(port).getAbsolutePath())
    }

  /** Delete the port number to which a scala compile server was connected */
  def deletePort(port: int): unit = portFile(port).delete()

  /** Get a socket connected to a daemon.  If create is true, then
    * create a new daemon if necessary.  Returns null if the connection
    * cannot be established.
    */
  def getOrCreateSocket(vmArgs: String, create: boolean): Socket = {
    val nAttempts = 49  // try for about 5 seconds
    def getsock(attempts: int): Socket =
      if (attempts == 0) {
        error("Unable to establish connection to compilation daemon")
        null
      } else {
        val port = if(create) getPort(vmArgs) else pollPort()
        if(port < 0) return null
        val hostName = InetAddress.getLocalHost().getHostName()
        try {
          val result = new Socket(hostName, port)
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

  def getSocket(hostName: String, port: int): Socket =
    try {
      new Socket(hostName, port)
    } catch {
      case e: /*IO+Security*/Exception =>
        fatal("Unable to establish connection to server " +
              hostName + ":" + port + "; exiting")
    }

  def getPassword(port: int): String = {
    val f = new BufferedReader(new FileReader(portFile(port)))
    val result = f.readLine()
    f.close()
    result
  }
}
