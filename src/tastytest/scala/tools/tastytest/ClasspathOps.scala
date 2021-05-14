package scala.tools.tastytest

import java.net.URL
import java.nio.file.Paths

object ClasspathOps {
  implicit class ClassPathSyntax(private val ls: List[String]) extends AnyVal {
    def asURLs: List[URL] = ls.map(Paths.get(_).toUri().toURL())
  }
}
