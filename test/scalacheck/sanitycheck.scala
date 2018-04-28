import java.io.File

import org.scalacheck._

object SanityCheck extends Properties("SanityCheck") {
  property("classpath correct") = {
    val codeSource = classOf[Option[_]].getProtectionDomain.getCodeSource.getLocation.toURI
    val path = new File(codeSource).getAbsolutePath
    val s = java.io.File.separator
    if (path.endsWith(s"quick${s}classes${s}library"))
      Prop.proved
    else
      Prop.falsified :| s"Unexpected code source for scala library: $path"
  }
}
