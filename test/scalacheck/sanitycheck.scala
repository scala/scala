import java.io.File

import org.scalacheck._

object SanityCheck extends Properties("SanityCheck") {
  property("classpath correct") = {
    val codeSource = classOf[Option[_]].getProtectionDomain.getCodeSource.getLocation.toURI
    val path = new File(codeSource).getAbsolutePath
    if (path.endsWith("quick/classes/library"))
      Prop.proved
    else
      Prop.falsified :| s"Unexpected code source for scala library: $path"
  }
}
