/* NSC -- new Scala compiler
 * Copyright 2009-2011 Scala Solutions and LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc
package interactive

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

  private def replError(msg: String) {
    reporter.error(/*new Position */FakePos("scalac"),
                   msg + "\n  scalac -help  gives more information")
  }

  def process(args: Array[String]) {
    val settings = new Settings(replError)
    reporter = new ConsoleReporter(settings)
    val command = new CompilerCommand(args.toList, settings)
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
    /*sys.*/exit(if (reporter.hasErrors) 1 else 0)// Don't use sys yet as this has to run on 2.8.2 also.
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
   *  typeat file off1 off2?
   *  complete file off1 off2?
   */
  def run(comp: Global) {
    val reloadResult = new Response[Unit]
    val typeatResult = new Response[comp.Tree]
    val completeResult = new Response[List[comp.Member]]
    val typedResult = new Response[comp.Tree]
    val structureResult = new Response[comp.Tree]

    def makePos(file: String, off1: String, off2: String) = {
      val source = toSourceFile(file)
      comp.rangePos(source, off1.toInt, off1.toInt, off2.toInt)
    }
    def doTypeAt(pos: Position) {
      comp.askTypeAt(pos, typeatResult)
      show(typeatResult)
    }
    def doComplete(pos: Position) {
      comp.askTypeCompletion(pos, completeResult)
      show(completeResult)
    }
    def doTypedTree(file: String) {
      comp.askType(toSourceFile(file), true, typedResult)
      show(typedResult)
    }
    def doStructure(file: String) {
      comp.askParsedEntered(toSourceFile(file), false, structureResult)
      show(structureResult)
    }

    loop { line =>
      (line split " ").toList match {
        case "reload" :: args =>
          comp.askReload(args map toSourceFile, reloadResult)
          show(reloadResult)
        case "reloadAndAskType" :: file :: millis :: Nil =>
          comp.askReload(List(toSourceFile(file)), reloadResult)
          Thread.sleep(millis.toInt)
          println("ask type now")
          comp.askType(toSourceFile(file), false, typedResult)
          typedResult.get
        case List("typed", file) =>
          doTypedTree(file)
        case List("typeat", file, off1, off2) =>
          doTypeAt(makePos(file, off1, off2))
        case List("typeat", file, off1) =>
          doTypeAt(makePos(file, off1, off1))
        case List("complete", file, off1, off2) =>
          doComplete(makePos(file, off1, off2))
        case List("complete", file, off1) =>
          doComplete(makePos(file, off1, off1))
        case List("quit") =>
          comp.askShutdown()
          // deleted sys. as this has to run on 2.8 also
          exit(1)
        case List("structure", file) =>
          doStructure(file)
        case _ =>
          println("unrecongized command")
      }
    }
  }

  def toSourceFile(name: String) = new BatchSourceFile(new PlainFile(new java.io.File(name)))

  def show[T](svar: Response[T]) {
    svar.get match {
      case Left(result) => println("==> "+result)
      case Right(exc) => exc.printStackTrace; println("ERROR: "+exc)
    }
    svar.clear()
  }
}
