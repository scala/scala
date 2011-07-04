package scala.tools.nsc.scratchpad

import java.io.OutputStream

class CommentOutputStream(out: CommentWriter, encoding: String = "") extends OutputStream {

  override def write(bs: Array[Byte]) =
    out.write(if (encoding.isEmpty) new String(bs) else new String(bs, encoding))

  override def write(bs: Array[Byte], off: Int, len: Int) =
    out.write(if (encoding.isEmpty) new String(bs, off, len) else new String(bs, off, len, encoding))

  override def write(ch: Int) =
    write(Array(ch.toByte))

  override def close() = out.close()
  override def flush() = out.flush()
}