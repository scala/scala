package scala.tools

import java.io.File
import java.nio.file.Path

package object xsbt {
  implicit class PathOps(private val path: Path) extends AnyVal {
    def / (sub: String) = path.resolve(sub)
  }

  implicit def pathToFile(path: Path): File = path.toFile
}
