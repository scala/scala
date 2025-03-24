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

import scala.util.Using
import scala.io.Source

import SourceFile._
import scala.util.chaining._

final case class SourceFile(path: String) {
  lazy val options: Options = readOptions(path)
}

object SourceFile {

  private val directivePattern = raw"\s*//>\s+using\s+(\S+)(?:\s+(.*))?".r
  final case class Options(data: Map[String, Option[String]])

  def readOptions(path: String): Options =
    Using.resource(Source.fromFile(path)) { source =>
      source.getLines().takeWhile(_.trim.startsWith("//>"))
        .flatMap {
          case directivePattern(key, valueOrNull) => Some(key -> Option(valueOrNull))
          case _ => None
        }
        .toMap
        .pipe(Options(_))
    }
}
