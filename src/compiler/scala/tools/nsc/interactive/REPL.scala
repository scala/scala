/* NSC -- new Scala compiler
 * Copyright 2009-2013 Typesafe/Scala Solutions and LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc
package interactive

import scala.concurrent.SyncVar
import scala.reflect.internal.util._
import scala.tools.nsc.symtab._
import scala.tools.nsc.ast._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.scratchpad.SourceInserter
import scala.tools.nsc.interpreter.AbstractFileClassLoader
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
      reporter.echo(versionMsg)
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
          reporter.echo(command.getInfoMessage(compiler))
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
    @deprecated("SI-6458: Instrumentation logic will be moved out of the compiler.","2.10.0")
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

    def doStructure(file: String) {
      comp.askParsedEntered(toSourceFile(file), false, structureResult)
      show(structureResult)
    }

    /** Write instrumented source file to disk.
     *  @param iFullName  The full name of the first top-level object in source
     *  @param iContents  An Array[Char] containing the instrumented source
     *  @return The name of the instrumented source file
     */
    @deprecated("SI-6458: Instrumentation logic will be moved out of the compiler.","2.10.0")
    def writeInstrumented(iFullName: String, suffix: String, iContents: Array[Char]): String = {
      val iSimpleName = iFullName drop ((iFullName lastIndexOf '.') + 1)
      val iSourceName = iSimpleName + suffix
      val ifile = new FileWriter(iSourceName)
      ifile.write(iContents)
      ifile.close()
      iSourceName
    }

    /** The method for implementing worksheet functionality.
     *  @param arguments  a file name, followed by optional command line arguments that are passed
     *                    to the compiler that processes the instrumented source.
     *  @param line       A line number that controls uop to which line results should be produced
     *                    If line = -1, results are produced for all expressions in the worksheet.
     *  @return           The generated file content containing original source in the left column
     *                    and outputs in the right column, or None if the presentation compiler
     *                    does not respond to askInstrumented.
     */
    @deprecated("SI-6458: Instrumentation logic will be moved out of the compiler.","2.10.0")
    def instrument(arguments: List[String], line: Int): Option[(String, String)] = {
      val source = toSourceFile(arguments.head)
      // strip right hand side comment column and any trailing spaces from all lines
      val strippedContents = SourceInserter.stripRight(source.content)
      val strippedSource = new BatchSourceFile(source.file, strippedContents)
      println("stripped source = "+strippedSource+":"+strippedContents.mkString)
      comp.askReload(List(strippedSource), reloadResult)
      comp.askInstrumented(strippedSource, line, instrumentedResult)
      using(instrumentedResult) {
        case (iFullName, iContents) =>
          println(s"instrumented source $iFullName = ${iContents.mkString}")
          val iSourceName = writeInstrumented(iFullName, "$instrumented.scala", iContents)
          val sSourceName = writeInstrumented(iFullName, "$stripped.scala", strippedContents)
          (iSourceName, sSourceName)
/*
 *           val vdirOpt = compileInstrumented(iSourceName, arguments.tail)
          runInstrumented(vdirOpt, iFullName, strippedSource.content)
 */
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
          comp.askLoadedTyped(toSourceFile(file), typedResult)
          typedResult.get
        case List("typeat", file, off1, off2) =>
          doTypeAt(makePos(file, off1, off2))
        case List("typeat", file, off1) =>
          doTypeAt(makePos(file, off1, off1))
        case List("complete", file, off1, off2) =>
          doComplete(makePos(file, off1, off2))
        case List("complete", file, off1) =>
          doComplete(makePos(file, off1, off1))
        case "instrument" :: arguments =>
          println(instrument(arguments, -1))
        case "instrumentTo" :: line :: arguments =>
          println(instrument(arguments, line.toInt))
        case List("quit") =>
          comp.askShutdown()
          exit(1) // Don't use sys yet as this has to run on 2.8.2 also.
        case List("structure", file) =>
          doStructure(file)
        case _ =>
          print("""Available commands:
                  | reload <file_1> ... <file_n>
                  | reloadAndAskType <file> <sleep-ms>
                  | typed <file>
                  | typeat <file> <start-pos> <end-pos>
                  | typeat <file> <pos>
                  | complete <file> <start-pos> <end-pos>
                  | compile <file> <pos>
                  | instrument <file> <arg>*
                  | instrumentTo <line-num> <file> <arg>*
                  | structure <file>
                  | quit
                  |""".stripMargin)
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
