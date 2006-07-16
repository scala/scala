/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc

import java.io._
import java.net._

object CompileSocket {

  /** The prefix of the port identification file, which is followed by the port number */
  private val dirName = "scalac-compile-server-port"

  /** The vm-part of the command to start a new scala compile server */
  private val vmCommand =
    System.getProperty("scala.home") match {
      case null => "scala"
      case dirname =>
        val trial = new File(new File(dirname, "bin"), "scala")
        if(trial.canRead)
          trial.getPath
        else
          "scala"
    }

  /** The class name of the scala compile server */
  private val serverClass = "scala.tools.nsc.CompileServer"

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
      if(topdir == null)
        return None

      val fulldir =
        extensions.foldLeft[File](new File(topdir))(
            (dir,ext)=>new File(dir, ext))

      Some(fulldir)
    }

    /** Write a test file to a directory to see if it is writable */
    def dirWritable(dir: File): Boolean = {
      dir.mkdirs
      if(!dir.exists)
        return false

      val f = new File(dir, "caniwrite")

      try {
        f.createNewFile
        if(!f.canWrite)
          return false
        f.delete
        if(f.exists)
          return false
      } catch {
        case _:java.io.IOException =>
          f.delete
          return false
      }

      return true
    }

    val potentials =
      for {
        val trial <- totry
        val expanded = expand(trial)
        !expanded.isEmpty
        dirWritable(expanded.get)
      }
      yield expanded.get

    if(potentials.isEmpty) {
      Console.println("could not find a directory for port files")
      exit(1)
    } else {
      val d = new File(potentials.head, dirName)
      d.mkdirs
      d
    }
  }

  /** Maximum number of polls for an available port */
  private val MaxAttempts = 100

  /** Time (in ms) to sleep between two polls */
  private val sleepTime = 20

  /** The command which starts the compile server, given vm arguments.
    * Multiple responses are given because different commands are needed
    * on different platforms; each possibility should be tried in order.
    *
    *  @param vmArgs  the argument string to be passed to the java or scala command
    *                 the string must be either empty or start with a ' '.
    */
  def serverCommands(vmArgs: String): List[String] =
    List(
      vmCommand+vmArgs+" "+serverClass,
      vmCommand+".bat"+vmArgs+" "+serverClass)

  /** Start a new server; returns true iff it succeeds */
  def startNewServer(vmArgs: String): Boolean = {
    for(val cmd <- serverCommands(vmArgs)) {
      try {
        Runtime.getRuntime().exec(cmd)
        return true
      } catch {
        case e:IOException => {
          Console.println(e)
          ()
        }
      }
    }
    return false
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
        case ex: Throwable =>
          System.err.println(ex)
          System.err.println("bad file in temp directory: "+hits(0).getAbsolutePath())
          System.err.println("please remove the file and try again")
          exit(1)
      }
  }


  /** Get the port number to which a scala compile server is connected;
   *  If no server is running yet, create one
   */
  def getPort(vmArgs: String): int = {
    var attempts = 0;
    var port = pollPort()
    if (port < 0) {
      if(!startNewServer(vmArgs)) {
        System.err.println("cannot start server.  tried commands:")
        for(val cmd <- serverCommands(vmArgs))
          System.err.println(cmd)
        exit(1)
      }
    }
    while (port < 0 && attempts < MaxAttempts) {
      attempts = attempts + 1
      Thread.sleep(sleepTime)
      port = pollPort()
    }
    if (port < 0) {
      Console.println("Could not connect to server.")
      exit(1)
    }
    port
  }

  /** Set the port number to which a scala compile server is connected */
  def setPort(port: int): unit =
    try {
      val f = new PrintWriter(new FileOutputStream(portFile(port)))
      f.println(new java.util.Random().nextInt.toString)
      f.close()
    } catch {
      case ex: IOException =>
        System.err.println("cannot create file: "+portFile(port).getAbsolutePath()+"; exiting")
        throw new Error()
    }

  /** Delete the port number to which a scala compile server was connected */
  def deletePort(port: int): unit = portFile(port).delete()

  def getOrCreateSocket(vmArgs: String): Socket = {
    val nAttempts = 9;
    def getsock(attempts: int): Socket =
      if (attempts == 0) {
        System.err.println("unable to establish connection to server; exiting");
        exit(1)
      } else {
        val port = getPort(vmArgs)
        val hostName = InetAddress.getLocalHost().getHostName()
        try {
          new Socket(hostName, port)
        } catch {
          case e: IOException =>
            System.err.println(e)
            System.err.println("...connection attempt to server at port "+port+" failed; re-trying...")
            if (attempts % 2 == 0) portFile(port).delete()
            Thread.sleep(100)
            val result = getsock(attempts - 1)
            if (attempts == nAttempts)
              System.err.println("...connection established at port "+port+"...")
            result
        }
      }
    getsock(nAttempts)
  }

  def getSocket(serverAdr: String): Socket = {
    val cpos = serverAdr indexOf ':'
    if (cpos < 0) {
      System.err.println("malformed server address: "+serverAdr+"; exiting")
      exit(1)
    } else {
      val hostName = serverAdr.substring(0, cpos)
      val port = try {
        Integer.parseInt(serverAdr.substring(cpos+1))
      } catch {
        case ex: Throwable =>
          System.err.println("malformed server address: "+serverAdr+"; exiting")
        exit(1)
      }
      getSocket(hostName, port)
    }
  }

  def getSocket(hostName: String, port: int): Socket =
    try {
      new Socket(hostName, port)
    } catch {
      case e: IOException =>
        System.err.println(
          "unable to establish connection to server "+hostName+":"+port+"; exiting")
        exit(1)
    }

  def getPassword(port: int): String = {
    val f = new BufferedReader(new FileReader(portFile(port)))
    val result = f.readLine()
    f.close()
    result
  }
}
