/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools.nsc
package interactive

import scala.reflect.internal.util._
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

  private def replError(msg: String): Unit = {
    reporter.error(/*new Position */FakePos("scalac"),
                   msg + "\n  scalac -help  gives more information")
  }

  def process(args: Array[String]): Unit = {
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
          if (true || command.settings.isDebug) // !!!
            ex.printStackTrace()
          reporter.error(null, "fatal error: " + msg)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    process(args)
    System.exit(if (reporter.hasErrors) 1 else 0)
  }

  def loop(action: (String) => Unit): Unit = {
    Console.print(prompt)
    try {
      val line = scala.io.StdIn.readLine()
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
  def run(comp: Global): Unit = {
    val reloadResult = new Response[Unit]
    val typeatResult = new Response[comp.Tree]
    val completeResult = new Response[List[comp.Member]]
    val typedResult = new Response[comp.Tree]
    val structureResult = new Response[comp.Tree]

    def makePos(file: String, off1: String, off2: String) = {
      val source = toSourceFile(file)
      comp.rangePos(source, off1.toInt, off1.toInt, off2.toInt)
    }

    def doTypeAt(pos: Position): Unit = {
      comp.askTypeAt(pos, typeatResult)
      show(typeatResult)
    }

    def doComplete(pos: Position): Unit = {
      comp.askTypeCompletion(pos, completeResult)
      show(completeResult)
    }

    def doStructure(file: String): Unit = {
      comp.askParsedEntered(toSourceFile(file), keepLoaded = false, structureResult)
      show(structureResult)
    }

    loop { line =>
      (line split " ").toList match {
        case "reload" :: args =>
          comp.askReload(args map toSourceFile, reloadResult)
          show(reloadResult)
        case "reloadAndAskType" :: file :: millis :: Nil =>
          comp.askReload(List(toSourceFile(file)), reloadResult)
          Thread.sleep(millis.toLong)
          println("ask type now")
          comp.askLoadedTyped(toSourceFile(file), keepLoaded = true, typedResult)
          typedResult.get
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
          System.exit(1)
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
