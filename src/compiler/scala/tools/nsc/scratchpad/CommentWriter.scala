package scala.tools.nsc.scratchpad

import java.io.Writer
import reflect.internal.Chars._


class CommentWriter(underlying: SourceInserter, startCol: Int = 40, endCol: Int = 152) extends Writer {

  private def rightCol(marker: String) = {
    while (underlying.column < startCol) underlying.write(' ')
    underlying.write(marker)
  }

  private var lastWasNL = false

  private def writeChar(ch: Char) = {
    if (underlying.column >= endCol) {
      underlying.write('\n'); rightCol("//| ")
    }
    if (underlying.column < startCol) rightCol("//> ")
    underlying.write(ch)
    lastWasNL = isLineBreakChar(ch)
  }

  override def write(chs: Array[Char], off: Int, len: Int) = {
    for (i <- off until off + len) writeChar(chs(i))
    flush()
  }

  def skip(len: Int) {
    if (lastWasNL) {
      underlying.backspace()
      lastWasNL = false
    }
    underlying.skip(len)
    if (underlying.column >= startCol) underlying.write('\n')
  }

  override def close() = underlying.close()
  override def flush() = underlying.flush()
}

