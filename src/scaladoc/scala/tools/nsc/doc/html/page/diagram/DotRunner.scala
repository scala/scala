package scala.tools.nsc
package doc
package html
package page
package diagram

import java.io.InputStream
import java.io.OutputStream
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.io.BufferedWriter
import java.io.BufferedReader
import scala.sys.process._
import scala.concurrent.SyncVar

import model._

/** This class takes care of running the graphviz dot utility */
class DotRunner(settings: doc.Settings) {

  private[this] var dotAttempts = 0
  private[this] var dotProcess: DotProcess  = null

  def feedToDot(dotInput: String, template: DocTemplateEntity): String = {

    if (dotProcess == null) {
      if (dotAttempts < settings.docDiagramsDotRestart.value + 1) {
        if (dotAttempts > 0)
          settings.printMsg("Graphviz will be restarted...\n")
        dotAttempts += 1
        dotProcess = new DotProcess(settings)
      } else
        return null
    }

    val tStart = System.currentTimeMillis
    val result = dotProcess.feedToDot(dotInput, template.qualifiedName)
    val tFinish = System.currentTimeMillis
    DiagramStats.addDotRunningTime(tFinish - tStart)

    if (result == null) {
      dotProcess.cleanup()
      dotProcess = null
      if (dotAttempts == 1 + settings.docDiagramsDotRestart.value) {
        settings.printMsg("\n")
        settings.printMsg("**********************************************************************")
        settings.printMsg("Diagrams will be disabled for this run because the graphviz dot tool")
        settings.printMsg("has malfunctioned too many times. These scaladoc flags may help:")
        settings.printMsg("")
        val baseList = List(settings.docDiagramsDebug,
                            settings.docDiagramsDotPath,
                            settings.docDiagramsDotRestart,
                            settings.docDiagramsDotTimeout)
        val width    = (baseList map (_.helpSyntax.length)).max
        def helpStr(s: doc.Settings#Setting) = ("%-" + width + "s") format (s.helpSyntax) + "  " + s.helpDescription
        baseList.foreach((sett: doc.Settings#Setting) => settings.printMsg(helpStr(sett)))
        settings.printMsg("\nPlease note that graphviz package version 2.26 or above is required.")
        settings.printMsg("**********************************************************************\n\n")

      }
    }

    result
  }

  def cleanup() =
    if (dotProcess != null)
      dotProcess.cleanup()
}

class DotProcess(settings: doc.Settings) {

  @volatile var error: Boolean = false           // signal an error
  val inputString = new SyncVar[String]                 // used for the dot process input
  val outputString = new SyncVar[String]                // used for the dot process output
  val errorBuffer: StringBuffer = new StringBuffer() // buffer used for both dot process error console AND logging

  // set in only one place, in the main thread
  var process: Process = null
  var templateName: String = ""
  var templateInput: String = ""

  def feedToDot(input: String, template: String): String = {

    templateName = template
    templateInput = input

    try {

      // process creation
      if (process == null) {
        val procIO = new ProcessIO(inputFn(_), outputFn(_), errorFn(_))
        val processBuilder: ProcessBuilder = Seq(settings.docDiagramsDotPath.value, "-Tsvg")
        process = processBuilder.run(procIO)
      }

      // pass the input and wait for the output
      assert(!inputString.isSet)
      assert(!outputString.isSet)
      inputString.put(input)
      var result = outputString.take(settings.docDiagramsDotTimeout.value * 1000L)
      if (error) result = null

      result

    } catch {
      case exc: Throwable =>
        errorBuffer.append("  Main thread in " + templateName + ": " +
          (if (exc.isInstanceOf[NoSuchElementException]) "Timeout" else "Exception: " + exc))
        error = true
        return null
    }
  }

  def cleanup(): Unit = {

    // we'll need to know if there was any error for reporting
    val _error = error

    if (process != null) {
      // if there's no error, this should exit cleanly
      if (!error) feedToDot("<finish>", "<finishing>")

      // just in case there's any thread hanging, this will take it out of the loop
      error = true
      process.destroy()
      // we'll need to unblock the input again
      if (!inputString.isSet) inputString.put("")
      if (outputString.isSet) outputString.take()
    }

    if (_error) {
      if (settings.docDiagramsDebug.value) {
        settings.printMsg("\n**********************************************************************")
        settings.printMsg("The graphviz dot diagram tool has malfunctioned and will be restarted.")
        settings.printMsg("\nThe following is the log of the failure:")
        settings.printMsg(errorBuffer.toString)
        settings.printMsg("  Cleanup: Last template: " + templateName)
        settings.printMsg("  Cleanup: Last dot input: \n    " + templateInput.replaceAll("\n","\n    ") + "\n")
        settings.printMsg("  Cleanup: Dot path: " + settings.docDiagramsDotPath.value)
        if (process != null)
          settings.printMsg("  Cleanup: Dot exit code: " + process.exitValue)
        settings.printMsg("**********************************************************************")
      } else {
        // we shouldn't just sit there for 50s not reporting anything, no?
        settings.printMsg("Graphviz dot encountered an error when generating the diagram for:")
        settings.printMsg(templateName)
        settings.printMsg("These are usually spurious errors, but if you notice a persistent error on")
        settings.printMsg("a diagram, please use the " + settings.docDiagramsDebug.name + " flag and report a bug with the output.")
      }
    }
  }

  /* The standard input passing function */
  private[this] def inputFn(stdin: OutputStream): Unit = {
    val writer = new BufferedWriter(new OutputStreamWriter(stdin))
    try {
      var input = inputString.take()

      while (!error) {
        if (input == "<finish>") {
          // empty => signal to finish
          stdin.close()
          return
        } else {
          // send output to dot
          writer.write(input + "\n\n")
          writer.flush()
        }

        if (!error) input = inputString.take()
      }
      stdin.close()
    } catch {
      case exc: Throwable =>
        error = true
        stdin.close()
        errorBuffer.append("  Input thread in " + templateName + ": Exception: " + exc + "\n")
    }
  }

  private[this] def outputFn(stdOut: InputStream): Unit = {
    val reader = new BufferedReader(new InputStreamReader(stdOut))
    val buffer: StringBuilder = new StringBuilder()
    try {
      var line = reader.readLine
      while (!error && line != null) {
        buffer.append(line + "\n")
        // signal the last element in the svg (only for output)
        if (line == "</svg>") {
          outputString.put(buffer.toString)
          buffer.setLength(0)
        }
        if (error) { stdOut.close(); return }
        line = reader.readLine
      }
      assert(!outputString.isSet)
      outputString.put(buffer.toString)
      stdOut.close()
    } catch {
      case exc: Throwable =>
        error = true
        stdOut.close()
        errorBuffer.append("  Output thread in " + templateName + ": Exception: " + exc + "\n")
    }
  }

  private[this] def errorFn(stdErr: InputStream): Unit = {
    val reader = new BufferedReader(new InputStreamReader(stdErr))
    try {
      var line = reader.readLine
      while (line != null) {
        errorBuffer.append("  DOT <error console>: " + line + "\n")
        error = true
        line = reader.readLine
      }
      stdErr.close()
    } catch {
      case exc: Throwable =>
        error = true
        stdErr.close()
        errorBuffer.append("  Error thread in " + templateName + ": Exception: " + exc + "\n")
    }
  }
}
