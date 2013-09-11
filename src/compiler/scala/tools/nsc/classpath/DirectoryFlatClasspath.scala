package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import java.io.File

case class DirectoryFlatClasspath(dir: File) extends AbstractFileFlatClasspath(AbstractFile.getDirectory(dir))
