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

  def normalize(args: Array[String]): Pair[String, String] = {
    def absFileName(path: String) = new File(path).getAbsolutePath()
    def absFileNames(paths: String) = {
      def afns(sep: char): String =
        StringOps.decompose(paths, sep)
          .map(absFileName)
          .mkString("", String.valueOf(sep), "")
      if (paths.indexOf(';') > 0) afns(';')
      else if (paths.indexOf(':') > 0) afns(':')
      else absFileName(paths)
    }
    var i = 0
    val vmArgs = new StringBuffer
    var serverAdr = ""
    while (i < args.length) {
      val arg = args(i)
      if (arg endsWith ".scala") {
        args(i) = absFileName(arg)
      } else if (arg startsWith "-J") {
        vmArgs append " -"+arg.substring(2)
        args(i) = ""
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

  def main(args: Array[String]): unit = {
    val Pair(vmArgs, serverAdr) = normalize(args)
    if (args.toList contains "-verbose") {
      System.out.println("[Server arguments: " + args.mkString("", " ", "]"))
      System.out.println("[VM arguments: " + vmArgs + "]")
    }
    val socket = if (serverAdr == "") CompileSocket.getOrCreateSocket(vmArgs)
                 else CompileSocket.getSocket(serverAdr)
    val out = new PrintWriter(socket.getOutputStream(), true)
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
    out.println(CompileSocket.getPassword(socket.getPort()))
    out.println(args.mkString("", "\0", ""))
    var fromServer = in.readLine()
    while (fromServer != null) {
      System.out.println(fromServer)
      fromServer = in.readLine()
    }
    in.close()
    out.close()
    socket.close()
  }
}
