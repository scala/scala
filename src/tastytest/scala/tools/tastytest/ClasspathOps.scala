/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.tastytest

import java.net.URL
import java.nio.file.Paths

object ClasspathOps {
  implicit class ClassPathSyntax(private val ls: List[String]) extends AnyVal {
    def asURLs: List[URL] = ls.map(Paths.get(_).toUri().toURL())
  }
}
