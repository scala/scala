/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package xsbt

import java.io.File
import java.nio.file.Path

/**
 * This is a utility class that provides a set of functions that
 * are used to implement straight to jar compilation.
 *
 * `sbt.internal.inc.JarUtils` is an object that has similar purpose and
 * duplicates some of the code, as it is difficult to share it. Any change
 * in the logic of this file must be applied to the other `JarUtils` too!
 */
final class JarUtils(outputDirs: Iterable[Path]) {
  // This is an equivalent of asking if it runs on Windows where the separator is `\`
  private val isSlashSeparator: Boolean = File.separatorChar == '/'

  /**
   * The jar file that is used as output for classes. If the output is
   * not set to a single .jar file, value of this field is [[None]].
   */
  val outputJar: Option[Path] = {
    outputDirs match {
      case Seq(file) if file.toString.endsWith(".jar") => Some(file)
      case _                                           => None
    }
  }

  /**
   * Creates an identifier for a class located inside a jar.
   *
   * It follows the format to encode inter-jar dependencies that
   * is established in [[sbt.internal.inc.JarUtils.ClassInJar]].
   */
  def classNameInJar(jar: Path, classFilePath: String): String = {
    s"$jar!${if (isSlashSeparator) classFilePath else classFilePath.replace('/', File.separatorChar)}"
  }
}
