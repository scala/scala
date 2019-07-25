package scala.tools.nsc.plugins

import scala.reflect.io.AbstractFile

trait OutputFileWriter {
  def writeFile(relativeName: String, data: Array[Byte], outputDir: AbstractFile)
}
