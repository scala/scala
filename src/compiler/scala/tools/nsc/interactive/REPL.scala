package scala.tools.nsc.interactive

import scala.concurrent.SyncVar
import scala.tools.nsc.util._
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

/** Interface of interactive compiler to a client such as an IDE
 */
object REPL extends EvalLoop {

  val settings = new Settings()
  val comp = new Global(settings, new ConsoleReporter(settings))

  def prompt = "> "

  /** Commands:
   *
   *  reload file1 ... fileN
   *  typeat file line col
   *
   *
   */
  def run() {
    val reloadResult = new SyncVar[Either[Unit, Throwable]]
    val typeatResult = new SyncVar[Either[comp.Tree, Throwable]]
    loop { line =>
      (line split " ").toList match {
        case "reload" :: args =>
          comp.askReload(args map toSourceFile, reloadResult)
          show(reloadResult)
        case List("typeat", file, line, col1, col2) =>
          val source = toSourceFile(file)
          val linestart = source.lineToOffset(line.toInt)
          val pos = comp.rangePos(source, linestart + col1.toInt, linestart + col1.toInt, linestart + col2.toInt)
          comp.askTypeAt(pos, typeatResult)
          show(typeatResult)
        case List("quit") =>
          System.exit(1)
        case _ =>
          println("unrecongized command")
      }
    }
  }

  def toSourceFile(name: String) = new BatchSourceFile(new PlainFile(new java.io.File(name)))

  def show[T](svar: SyncVar[Either[T, Throwable]]) {
    svar.get match {
      case Left(result) => println("==> "+result)
      case Right(exc/*: Throwable ??*/) => exc.printStackTrace; println("ERROR: "+exc)
    }
  }

  def main(args: Array[String]) {
    run()
  }
}
