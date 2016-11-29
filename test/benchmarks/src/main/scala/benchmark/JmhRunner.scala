package benchmark

import java.io.File

/** Common code for JMH runner objects. */
trait JmhRunner {
  private[this] val parentDirectory = new File("target", "jmh-results")

  /** Return the output directory for this class, creating the directory if necessary. */
  protected def outputDirectory: File = {
    val subdir = getClass.getPackage.getName.replace('.', File.separatorChar)
    val dir = new File(parentDirectory, subdir)
    if (!dir.isDirectory) dir.mkdirs()
    dir
  }
}
