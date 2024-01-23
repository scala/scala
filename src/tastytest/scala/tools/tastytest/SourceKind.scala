/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.tastytest

sealed abstract class SourceKind(val name: String)(val filter: String => Boolean = _.endsWith(name)) { self =>
  def fileOf(name: String) = name + self.name
}

object SourceKind {

  case object NoSource  extends SourceKind("")(filter = _ => false)
  case object Scala     extends SourceKind(".scala")()
  case object ScalaFail extends SourceKind("_fail.scala")()
  case object ScalaPre  extends SourceKind("_pre.scala")()
  case object Check     extends SourceKind(".check")()
  case object SkipCheck extends SourceKind(".skipcheck")()
  case object Java      extends SourceKind(".java")()
  case object TastyFile extends SourceKind(".tasty")()

  def filterByKind(kinds: Set[SourceKind], paths: String*): Seq[String] =
    if (kinds.isEmpty) Nil
    else paths.filter(kinds.foldLeft(NoSource.filter)((filter, kind) => p => kind.filter(p) || filter(p)))
}
