/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{Writer, PrintWriter, Reader, BufferedReader,
                IOException, InputStream, StringWriter, InputStreamReader,
                OutputStreamWriter, StringReader, OutputStream}

object StreamAppender {

  def apply(reader: BufferedReader, writer: Writer): StreamAppender = {
    val pwriter = new PrintWriter(writer, true)
    new StreamAppender(reader, pwriter)
  }

  def apply(reader: Reader, writer: Writer): StreamAppender =
    apply(new BufferedReader(reader), writer)

  def appendToString(in1: InputStream, in2: InputStream): String = {
    val swriter1 = new StringWriter
    val swriter2 = new StringWriter
    val reader1 = new BufferedReader(new InputStreamReader(in1))
    val reader2 = new BufferedReader(new InputStreamReader(in2))
    val app1 = StreamAppender(reader1, swriter1)
    val app2 = StreamAppender(reader2, swriter2)

    val async = new Thread(app2)
    async.start()
    app1.run()
    async.join()
    swriter1.toString + swriter2.toString
  }
/*
  private def inParallel(t1: Runnable, t2: Runnable, t3: Runnable) {
    val thr1 = new Thread(t1)
    val thr2 = new Thread(t2)
    thr1.start()
    thr2.start()
    t3.run()
    thr1.join()
    thr2.join()
  }
*/
  private def inParallel(t1: Runnable, t2: Runnable) {
    val thr = new Thread(t2)
    thr.start()
    t1.run()
    thr.join()
  }

  def concat(in: InputStream, err: InputStream, out: OutputStream) = new Runnable {
    override def run() {
      val outWriter = new PrintWriter(new OutputStreamWriter(out), true)
      val inApp = new StreamAppender(new BufferedReader(new InputStreamReader(in)),
                                     outWriter)
      val errStringWriter = new StringWriter
      val errApp = StreamAppender(new BufferedReader(new InputStreamReader(err)),
                                  errStringWriter)
      inParallel(inApp, errApp)

      // append error string to out
      val errStrApp = new StreamAppender(new BufferedReader(new StringReader(errStringWriter.toString)),
                                         outWriter)
      errStrApp.run()
    }
  }
}

class StreamAppender(reader: BufferedReader, writer: PrintWriter) extends Runnable {
  override def run() = runAndMap(identity)

  def runAndMap(f: String => String) {
    try {
      var line = reader.readLine()
      while (line != null) {
        writer.println(f(line))
        line = reader.readLine()
      }
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }
}
