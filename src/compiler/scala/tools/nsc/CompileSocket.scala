/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io._
import java.net._

object CompileSocket {

  /** The prefix of the port identification file, which is followed
   *  by the port number.
   */
  private val dirName = "scalac-compile-server-port"

  private val isWin = System.getProperty("os.name") startsWith "Windows"
  private val cmdName = if (isWin) "scala.bat" else "scala"

  /** The vm-part of the command to start a new scala compile server */
  private val vmCommand =
    System.getProperty("scala.home") match {
      case null => cmdName
      case dirname =>
        val trial = new File(new File(dirname, "bin"), cmdName)
        if (trial.canRead)
          trial.getPath
        else
          cmdName
    }

  /** The class name of the scala compile server */
  private val serverClass = "scala.tools.nsc.CompileServer"

  private def fatal(msg: String) = {
    System.err.println(msg)
    exit(1)
  }

  private def info(msg: String) =
    if (CompileClient.verbose) System.out.println(msg)

  /** The temporary directory in which the port identification file is stored */
  private val tmpDir = {
    val totry = List(
        Pair("scala.home", List("var", "scala-devel")),
        Pair("user.home", List("tmp")),
        Pair("java.io.tmpdir", Nil))

    /** Expand a property-extensions pair into a complete File object */
    def expand(trial: Pair[String, List[String]]): Option[File] = {
      val Pair(topdirProp, extensions) = trial
      val topdir = System.getProperty(topdirProp)
      if (topdir == null)
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
      fatal("Could not find a directory for port files")
    else {
      val d = new File(potentials.head, dirName)
      info("[Temp directory: " + d + "]")
      d.mkdirs
      d
    }
  }

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
        fatal("cannot start server." +
              "\ntried command: " + cmd)
    }
  }

  /** The port identification file */
  def portFile(port: int) = new File(tmpDir, port.toString())

  /** Poll for a server port number; return -1 if none exists yet */
  private def pollPort(): int = {
    val hits = tmpDir.listFiles()
    if (hits.length == 0) -1
    else
      try {
        for (val i <- 1 until hits.length) hits(i).delete()
        Integer.parseInt(hits(0).getName)
      } catch {
        case ex: NumberFormatException =>
          fatal(ex.toString() +
                "\nbad file in temp directory: " +
                hits(0).getAbsolutePath() +
                "\nplease remove the file and try again")
      }
  }

  /** Get the port number to which a scala compile server is connected;
   *  If no server is running yet, create one
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
      fatal("Could not connect to server.")
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
              portFile(port).getAbsolutePath() + "; exiting")
    }

  /** Delete the port number to which a scala compile server was connected */
  def deletePort(port: int): unit = portFile(port).delete()

  def getOrCreateSocket(vmArgs: String): Socket = {
    val nAttempts = 9
    def getsock(attempts: int): Socket =
      if (attempts == 0)
        fatal("Unable to establish connection to server; exiting")
      else {
        val port = getPort(vmArgs)
        val hostName = InetAddress.getLocalHost().getHostName()
        try {
          new Socket(hostName, port)
        } catch {
          case e: /*IO+Security*/Exception =>
            System.err.println(e)
            System.err.println("...connection attempt to server at port " +
                               port + " failed; re-trying...")
            if (attempts % 2 == 0) portFile(port).delete()
            Thread.sleep(100)
            val result = getsock(attempts - 1)
            if (attempts == nAttempts)
              System.err.println("...connection established at port " +
                                 port + "...")
            result
        }
      }
    getsock(nAttempts)
  }

  def getSocket(serverAdr: String): Socket = {
    val cpos = serverAdr indexOf ':'
    if (cpos < 0)
      fatal("Malformed server address: " + serverAdr + "; exiting")
    else {
      val hostName = serverAdr.substring(0, cpos)
      val port = try {
        Integer.parseInt(serverAdr.substring(cpos+1))
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
