package scala.tools.nsc
import java.io.Writer

/** A Writer that writes onto the Scala Console */
class ConsoleWriter extends Writer {
  def close = flush

  def flush = Console.flush

  def write(cbuf: Array[char], off: int, len: int): Unit =
    if(len > 0)
      write(new String(cbuf.subArray(off, off+len-1)))

  override def write(str: String): Unit = Console.print(str)
}
