
import tools.partest.DirectTest

import java.io.File

// warn only if no plugin on Xplugin path
object Test extends DirectTest {
  override def code = "class Code"

  override def extraSettings = s"-usejavacp"

  override def show() = {
    val tmp = new File(testOutput.jfile, "plugins.partest").getAbsolutePath
    compile(s"-Xplugin:$tmp", "-Xpluginsdir", tmp)
  }
}

