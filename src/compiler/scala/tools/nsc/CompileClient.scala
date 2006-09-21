/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

import scala.tools.util.StringOps
import java.io._

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object CompileClient {
  val PRODUCT: String =
    System.getProperty("scala.tool.name", "scalac")
  val VERSION: String =
    System.getProperty("scala.tool.version", "unknown version")
  val COPYRIGHT: String =
    System.getProperty("scala.copyright", "(c) 2002-2006 LAMP/EPFL")

  val versionMsg = PRODUCT + " " + VERSION + " -- " + COPYRIGHT

  var verbose = false
  var version = false

  /** Convert a filename to an absolute path */
  def absFileName(path: String) = new File(path).getAbsolutePath()

  /** Convert a sequence of filenames, separated by File.pathSeparator,
    * into absolute filenames.
    */
  def absFileNames(paths: String) = {
    val sep = File.pathSeparator
    val pathsList = paths.split(sep).toList
    pathsList.map(absFileName).mkString("", sep, "")
  }

  def normalize(args: Array[String]): Pair[String, String] = {
    var i = 0
    val vmArgs = new StringBuffer
    var serverAdr = ""
    while (i < args.length) {
      val arg = args(i)
      if (arg endsWith ".scala") {
        args(i) = absFileName(arg)
      } else if (arg startsWith "-J") {
        //see http://java.sun.com/j2se/1.5.0/docs/tooldocs/solaris/javac.html#J
        vmArgs append " "+arg.substring(2)
        args(i) = ""
      } else if (arg == "-verbose") {
        verbose = true
      } else if (arg == "-version") {
        version = true
      }
      i = i + 1
      if (i < args.length) {
        if (arg == "-classpath" ||
            arg == "-sourcepath" ||
            arg == "-bootclasspath" ||
            arg == "-extdirs" ||
            arg == "-d") {
          args(i) = absFileNames(args(i))
          i = i + 1
        } else if (arg == "-server") {
          serverAdr = args(i)
          args(i-1) = ""
          args(i) = ""
        }
      }
    }
    Pair(vmArgs.toString, serverAdr)
  }

  def main(args0: Array[String]): unit = {
    val args =
      if(args0.exists(arg => arg=="-d"))
        args0
      else
        ("-d" :: "." :: args0.toList).toArray

    val Pair(vmArgs, serverAdr) = normalize(args)
    if(version) {
      System.out.println(versionMsg)
      return
    }
    if (verbose) {
      System.out.println("[Server arguments: " + args.mkString("", " ", "]"))
      System.out.println("[VM arguments: " + vmArgs + "]")
    }
    val socket = if (serverAdr == "") CompileSocket.getOrCreateSocket(vmArgs)
                 else CompileSocket.getSocket(serverAdr)
    val out = new PrintWriter(socket.getOutputStream(), true)
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
    out.println(CompileSocket.getPassword(socket.getPort()))
    out.println(args.mkString("", "\0", ""))
    var sawerror = false
    var fromServer = in.readLine()
    while (fromServer != null) {
      if(CompileSocket.errorPattern.matcher(fromServer).matches)
        sawerror = true
      System.out.println(fromServer)
      fromServer = in.readLine()
    }
    in.close()
    out.close()
    socket.close()

    System.exit(if (sawerror) 1 else 0)
  }
}
