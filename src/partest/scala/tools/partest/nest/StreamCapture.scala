/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Paul Phillips
 */
package scala.tools.partest
package nest

import java.io.{Console => _, _}
import java.nio.charset.Charset

object StreamCapture {
  def savingSystem[T](body: => T): T = {
    val savedOut  = System.out
    val savedErr  = System.err
    try body
    finally {
      System setErr savedErr
      System setOut savedOut
    }
  }

  def capturingOutErr[A](output: OutputStream)(f: => A): A = {
    import java.io._
    val charset = Charset.defaultCharset()
    val printStream = new PrintStream(output, true, charset.name())
    savingSystem {
      System.setOut(printStream)
      System.setErr(printStream)
      try {
        scala.Console.withErr(printStream) {
          scala.Console.withOut(printStream) {
            f
          }
        }
      } finally {
        printStream.close()
      }
    }
  }

  def withExtraProperties[A](extra: Map[String, String])(action: => A): A = {
    val saved = System.getProperties()
    val modified = new java.util.Properties()
    modified.putAll(saved)
    extra.foreach { case (k, v) => modified.setProperty(k, v) }
    // Trying to avoid other threads seeing the new properties object prior to the new entries
    // https://github.com/scala/scala/pull/6391#issuecomment-371346171
    UnsafeAccess.U.storeFence()
    System.setProperties(modified)
    try {
      action
    } finally {
      System.setProperties(saved)
    }
  }
}
