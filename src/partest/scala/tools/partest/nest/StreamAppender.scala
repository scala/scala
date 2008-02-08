/* NEST (New Scala Test)
 * @author Philipp Haller
 */

package scala.tools.partest.nest

import java.io.{Writer, PrintWriter, Reader, BufferedReader,
                IOException}

class StreamAppender(from: Reader, to: Writer) {
  private val thr = new Thread(new Runnable {
    def run() {
      try {
        val writer = new PrintWriter(to)
        val reader = new BufferedReader(from)
        var line = reader.readLine()
        while (line != null) {
          writer.println(line)
          line = reader.readLine()
        }
        writer.flush()
      } catch {
        case e: IOException =>
          e.printStackTrace()
      }
    }
  })

  def start() { thr.start() }
}
