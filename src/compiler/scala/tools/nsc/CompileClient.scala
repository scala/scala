/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import java.io.{BufferedReader, File, InputStreamReader, PrintWriter}
import Properties.fileEndings

/** The client part of the fsc offline compiler.  Instead of compiling
 *  things itself, it send requests to a CompileServer.
 */
class StandardCompileClient {
  def compileSocket: CompileSocket = CompileSocket  // todo: should be lazy val

  val versionMsg  = "Fast " + Properties.versionMsg
  var verbose     = false
  var version     = false
  var shutdown    = false

  /** Convert a filename to an absolute path */
  def absFileName(path: String) = new File(path).getAbsolutePath()

  /** Convert a sequence of filenames, separated by <code>File.pathSeparator</code>,
    * into absolute filenames.
    */
  def absFileNames(paths: String) = {
    val sep = File.pathSeparator
    val pathsList = paths.split(sep).toList
    pathsList map absFileName mkString sep
  }

  protected def normalize(args: Array[String]): (String, String) = {
    var i = 0
    val vmArgs = new StringBuilder
    var serverAdr = ""

    while (i < args.length) {
      val arg = args(i)
      if (fileEndings exists(arg endsWith _)) {
        args(i) = absFileName(arg)
      } else if (arg startsWith "-J") {
        //see http://java.sun.com/j2se/1.5.0/docs/tooldocs/solaris/javac.html#J
        vmArgs append " "+arg.substring(2)
        args(i) = ""
      } else if (arg == "-verbose") {
        verbose = true
      } else if (arg == "-version") {
        version = true
      } else if (arg == "-shutdown") {
        shutdown = true
      }
      i += 1

      if (i < args.length) {
        arg match {
          case "-classpath" | "-sourcepath" | "-bootclasspath" | "-extdirs" | "-d"  =>
            args(i) = absFileNames(args(i))
            i += 1
          case "-server"  =>
            serverAdr = args(i)
            args(i-1) = ""
            args(i) = ""
          case _          =>
        }
      }
    }
    (vmArgs.toString, serverAdr)
  }

  // used by class ant.FastScalac to skip exit statement in Ant.
  def main0(args0: Array[String]): Int = {
    val args = if (args0 contains "-d") args0 else Array("-d", ".") ++ args0
    val (vmArgs, serverAdr) = normalize(args)

    if (version) {
      Console.println(versionMsg)
      return 0
    }
    if (verbose) {
      Console.println("[Server arguments: " + args.mkString("", " ", "]"))
      Console.println("[VM arguments: " + vmArgs + "]")
    }
    val socket = if (serverAdr == "") compileSocket.getOrCreateSocket(vmArgs, !shutdown)
                 else compileSocket.getSocket(serverAdr)
    var sawerror = false
    if (socket eq null) {
      if (shutdown) {
        Console.println("[No compilation server running.]")
      } else {
        Console.println("Compilation failed.")
        sawerror = true
      }
    } else {
      val out = new PrintWriter(socket.getOutputStream(), true)
      val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
      out.println(compileSocket.getPassword(socket.getPort()))
      out.println(args.mkString("", "\0", ""))
      var fromServer = in.readLine()
      while (fromServer ne null) {
        if (compileSocket.errorPattern.matcher(fromServer).matches)
          sawerror = true
        Console.println(fromServer)
        fromServer = in.readLine()
      }
      in.close ; out.close ; socket.close
    }
    if (sawerror) 1 else 0
  }

  def main(args: Array[String]): Unit =
    exit(try main0(args) catch { case e: Exception => 1 })
}


object CompileClient extends StandardCompileClient
