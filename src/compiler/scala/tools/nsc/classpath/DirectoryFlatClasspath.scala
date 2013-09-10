package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile
import java.io.File

class DirectoryFlatClasspath(dir: File) extends AbstractFileFlatClasspath(AbstractFile.getDirectory(dir))
