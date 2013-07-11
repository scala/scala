package scala.tools.nsc.interactive

import java.io.Writer

class PrettyWriter(wr: Writer) extends Writer {
  protected val indentStep = "  "
  private var indent = 0
  private def newLine() {
    wr.write('\n')
    wr.write(indentStep * indent)
  }
  def close() = wr.close()
  def flush() = wr.flush()
  def write(str: Array[Char], off: Int, len: Int): Unit = {
    if (off < str.length && off < len) {
      str(off) match {
        case '{' | '[' | '(' =>
          indent += 1
          wr.write(str(off).toInt)
          newLine()
          wr.write(str, off + 1, len - 1)
        case '}' | ']' | ')' =>
          wr.write(str, off, len)
          indent -= 1
        case ',' =>
          wr.write(',')
          newLine()
          wr.write(str, off + 1, len - 1)
        case ':' =>
          wr.write(':')
          wr.write(' ')
          wr.write(str, off + 1, len - 1)
        case _ =>
          wr.write(str, off, len)
      }
    } else {
      wr.write(str, off, len)
    }
  }
  override def toString = wr.toString
}
