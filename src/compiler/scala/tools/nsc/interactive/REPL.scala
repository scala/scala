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
import scala.tools.nsc.scratchpad.{Executor, SourceInserter}
import java.io.{File, FileWriter}

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
    val instrumentedResult = new Response[(String, Array[Char])]

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

    /** This is the method for implement worksheet functionality
     */
    def instrument(source: SourceFile, line: Int): Option[Array[Char]] = {
      // strip right hand side comment column and any trailing spaces from all lines
      val strippedSource = new BatchSourceFile(source.file, SourceInserter.stripRight(source.content))
      comp.askReload(List(strippedSource), reloadResult)
      // todo: Display strippedSource in place of source
      comp.askInstrumented(strippedSource, line, instrumentedResult)
      using(instrumentedResult) { case (iFullName, iContents) =>
        // iFullName: The full name of the first top-level object in source
        // iContents: An Array[Char] containing the instrumented source
        // Create a file from iContents so that it can be compiled
        // The name here is <simple name of object>+"$instrumented.scala", but
        // it could be anything.
        val iSimpleName = iFullName drop ((iFullName lastIndexOf '.') + 1)
        val iSourceName = iSimpleName + "$instrumented.scala"
        val ifile = new FileWriter(iSourceName)
        ifile.write(iContents)
        ifile.close()
        println("compiling "+iSourceName)
        // Compile instrumented source
        scala.tools.nsc.Main.process(Array(iSourceName, "-verbose", "-d", "/classes"))
        // Run instrumented source, inserting all output into stripped source
        println("running "+iSimpleName)
        val si = new SourceInserter(strippedSource.content)
        Executor.execute(iSimpleName, si)
        println("done")
        si.currentContents
      }
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
        case List("instrument", file) =>
          println(instrument(toSourceFile(file), -1).map(_.mkString))
        case List("instrument", file, line) =>
          println(instrument(toSourceFile(file), line.toInt).map(_.mkString))
        case List("quit") =>
          comp.askShutdown()
          exit(1) // Don't use sys yet as this has to run on 2.8.2 also.
        case List("structure", file) =>
          doStructure(file)
        case _ =>
          println("unrecongized command")
      }
    }
  }

  def toSourceFile(name: String) = new BatchSourceFile(new PlainFile(new java.io.File(name)))

  def using[T, U](svar: Response[T])(op: T => U): Option[U] = {
    val res = svar.get match {
      case Left(result) => Some(op(result))
      case Right(exc) => exc.printStackTrace; println("ERROR: "+exc); None
    }
    svar.clear()
    res
  }

  def show[T](svar: Response[T]) = using(svar)(res => println("==> "+res))
}
