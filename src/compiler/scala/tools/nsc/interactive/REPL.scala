package scala.tools.nsc.interactive

import scala.concurrent.SyncVar
import scala.tools.nsc.util._
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

/** Interface of interactive compiler to a client such as an IDE
 */
object REPL {

  val versionMsg = "Scala compiler " +
    Properties.versionString + " -- " +
    Properties.copyrightString

  val prompt = "> "

  var reporter: ConsoleReporter = _

  def error(msg: String) {
    reporter.error(/*new Position */FakePos("scalac"),
                   msg + "\n  scalac -help  gives more information")
  }

  def process(args: Array[String]) {
    val settings = new Settings(error)
    reporter = new ConsoleReporter(settings)
    val command = new CompilerCommand(args.toList, settings, error, false)
    if (command.settings.version.value)
      reporter.info(null, versionMsg, true)
    else {
      try {
        object compiler extends Global(command.settings, reporter) {
//          printTypings = true
        }
        if (reporter.hasErrors) {
          reporter.flush()
          return
        }
        if (command.shouldStopWithInfo) {
          reporter.info(null, command.getInfoMessage(compiler), true)
        } else {
          run(compiler)
        }
      } catch {
        case ex @ FatalError(msg) =>
          if (true || command.settings.debug.value) // !!!
            ex.printStackTrace();
        reporter.error(null, "fatal error: " + msg)
      }
    }
  }

  def main(args: Array[String]) {
    process(args)
    exit(if (reporter.hasErrors) 1 else 0)
  }

  def loop(action: (String) => Unit) {
    Console.print(prompt)
    try {
      val line = Console.readLine
      if (line.length() > 0) {
        action(line)
      }
      loop(action)
    }
    catch {
      case _: java.io.EOFException => //nop
    }
  }

  /** Commands:
   *
   *  reload file1 ... fileN
   *  typeat file line col
   *
   *
   */
  def run(comp: Global) {
    val reloadResult = new SyncVar[Either[Unit, Throwable]]
    val typeatResult = new SyncVar[Either[comp.Tree, Throwable]]
    loop { line =>
      (line split " ").toList match {
        case "reload" :: args =>
          comp.askReload(args map toSourceFile, reloadResult)
          show(reloadResult)
        case "typeat" :: file :: line :: col1 :: col2 :: Nil =>
          val source = toSourceFile(file)
          val linestart = source.lineToOffset(line.toInt)
          val pos = comp.rangePos(source, linestart + col1.toInt, linestart + col1.toInt, linestart + col2.toInt)
          comp.askTypeAt(pos, typeatResult)
          show(typeatResult)
        case "quit" :: Nil =>
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
}
