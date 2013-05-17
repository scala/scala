/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Paul Phillips
 */
package scala.tools.partest
package nest

import java.io.{ Console => _, _ }

object StreamCapture {
  case class Captured[T](stdout: String, stderr: String, result: T) {
    override def toString = s"""
      |result: $result
      |[stdout]
      |$stdout
      |[stderr]
      |$stderr""".stripMargin.trim
  }

  private def mkStream = {
    val swr     = new StringWriter
    val wr      = new PrintWriter(swr, true)
    val ostream = new PrintStream(new OutputStream { def write(b: Int): Unit = wr write b }, true) // autoFlush = true

    (ostream, () => { ostream.close() ; swr.toString })
  }

  def savingSystem[T](body: => T): T = {
    val savedOut  = System.out
    val savedErr  = System.err
    try body
    finally {
      System setErr savedErr
      System setOut savedOut
    }
  }

  def apply[T](body: => T): Captured[T] = {
    val (outstream, stdoutFn) = mkStream
    val (errstream, stderrFn) = mkStream

    val result = savingSystem {
      System setOut outstream
      System setErr errstream
      Console.withOut(outstream) {
        Console.withErr(errstream) {
          body
        }
      }
    }
    Captured(stdoutFn(), stderrFn(), result)
  }
}
