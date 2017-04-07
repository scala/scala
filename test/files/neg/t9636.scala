
import java.io._
import java.util.zip._

class C {
  def isWrapper(is: FileInputStream): InputStream = {
    val pb = new PushbackInputStream(is, 2)
    val signature = new Array[Byte](2)
    pb.read(signature)
    pb.unread(signature)
    if (signature.sameElements(Array(0x1F, 0x8B))) {
      new GZIPInputStream(new BufferedInputStream(pb))
    } else {
      pb
    }
  }
}
