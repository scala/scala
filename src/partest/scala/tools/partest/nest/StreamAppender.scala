/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import java.io.{Writer, PrintWriter, Reader, BufferedReader,
                IOException, InputStream, StringWriter, InputStreamReader}

object StreamAppender {
  def appenderToWriter(reader: BufferedReader, writer: Writer) = {
    val pwriter = new PrintWriter(writer, true)
    new StreamAppender(reader, pwriter)
  }

  def appendToString(in1: InputStream, in2: InputStream): String = {
    val swriter1 = new StringWriter
    val swriter2 = new StringWriter
    val reader1 = new BufferedReader(new InputStreamReader(in1))
    val reader2 = new BufferedReader(new InputStreamReader(in2))
    val app1 = appenderToWriter(reader1, swriter1)
    val app2 = appenderToWriter(reader2, swriter2)

    val async = new Thread(app2)
    async.start()
    app1.run()
    async.join()
    swriter1.toString + swriter2.toString
  }
}

class StreamAppender(reader: BufferedReader, writer: PrintWriter) extends Runnable {
  override def run() {
    try {
      var line = reader.readLine()
      while (line != null) {
        writer.println(line)
        line = reader.readLine()
      }
    } catch {
      case e: IOException =>
        e.printStackTrace()
    }
  }
}
