/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Paul Phillips
 */
package scala.tools.partest
package nest

import java.io.{Console => _, _}
import java.nio.charset.Charset

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

  def withExtraProperties[A](extra: Map[String, String])(action: => A): A = {
    val saved = System.getProperties()
    val modified = new java.util.Properties()
    saved.stringPropertyNames().forEach((k) => modified.setProperty(k, saved.getProperty(k)))
    extra.foreach { case (k, v) => modified.setProperty(k, v) }
    System.setProperties(modified)
    try {
      action
    } finally {
      System.setProperties(saved)
    }
  }

  // TODO merge with code above
  def capturingOutErr[A](output: OutputStream)(f: => A): A = {
    import java.io._
    val savedOut = System.out
    val savedErr = System.err
    try {
      val charset = Charset.defaultCharset()
      val printStream = new PrintStream(output, true, charset.name())
      try {
        System.setOut(printStream)
        System.setErr(printStream)
        scala.Console.withErr(printStream) {
          scala.Console.withOut(printStream) {
            f
          }
        }
      } finally {
        printStream.close()
      }
    } finally {
      System.setOut(savedOut)
      System.setOut(savedErr)
    }
  }
}
