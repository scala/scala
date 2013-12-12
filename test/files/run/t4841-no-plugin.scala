
import tools.partest.DirectTest

import java.io.File

// warn only if no plugin on Xplugin path
object Test extends DirectTest {
  override def code = "class Code"

  override def extraSettings = s"-usejavacp -d ${testOutput.path}"

  override def show() = {
    val tmp = new File(testOutput.jfile, "plugins.partest").getAbsolutePath
    compile("-Xdev", s"-Xplugin:$tmp", "-Xpluginsdir", tmp)
  }
}

