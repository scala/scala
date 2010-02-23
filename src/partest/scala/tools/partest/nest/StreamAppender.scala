/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io._

object StreamAppender {
  def wrapIn(in: InputStream): BufferedReader = new BufferedReader(new InputStreamReader(in))
  def wrapIn(reader: Reader): BufferedReader = new BufferedReader(reader)
  def wrapIn(str: String): BufferedReader = new BufferedReader(new StringReader(str))

  def wrapOut(out: OutputStream): PrintWriter = new PrintWriter(new OutputStreamWriter(out), true)
  def wrapOut(writer: Writer): PrintWriter = new PrintWriter(writer, true)
  def wrapOut(): PrintWriter = wrapOut(new StringWriter)

  def apply(reader: BufferedReader, writer: Writer): StreamAppender =
    new StreamAppender(reader, wrapOut(writer))

  def apply(reader: Reader, writer: Writer): StreamAppender =
    apply(wrapIn(reader), writer)

  def apply(in: InputStream, writer: Writer): StreamAppender =
    apply(wrapIn(in), writer)

  def apply(str: String, writer: Writer): StreamAppender =
    apply(wrapIn(str), writer)

  def apply(in: File, out: File): StreamAppender =
    apply(new FileReader(in), new FileWriter(out))

  def appendToString(in1: InputStream, in2: InputStream): String = {
    val swriter1 = new StringWriter
    val swriter2 = new StringWriter
    val app1 = StreamAppender(wrapIn(in1), swriter1)
    val app2 = StreamAppender(wrapIn(in2), swriter2)

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
      val outWriter = wrapOut(out)
      val inApp = StreamAppender(in, outWriter)

      val errStringWriter = new StringWriter
      val errApp = StreamAppender(wrapIn(err), errStringWriter)

      inParallel(inApp, errApp)

      // append error string to out
      StreamAppender(errStringWriter.toString, outWriter).run()
    }
  }
}

class StreamAppender(reader: BufferedReader, writer: PrintWriter) extends Runnable {
  override def run() = runAndMap(identity)
  private def lines() = Iterator continually reader.readLine() takeWhile (_ != null)
  def closeAll() = {
    reader.close()
    writer.close()
  }

  def runAndMap(f: String => String) =
    try lines() map f foreach (writer println _)
    catch { case e: IOException => e.printStackTrace() }
}
