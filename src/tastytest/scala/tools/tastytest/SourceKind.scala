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

import scala.util.Properties

sealed abstract class SourceKind(val name: String){ self =>
  def permits(file: String): Boolean
  def shouldValidate: Boolean
  def validate(options: SourceFile.Options): Boolean
}

sealed trait PermitByName { self: SourceKind =>
  def permits(file: String): Boolean = file.endsWith(name)
  def fileOf(name: String) = name + self.name
}

sealed trait AlwaysValid { self: SourceKind =>
  def shouldValidate: Boolean = false
  def validate(options: SourceFile.Options) = true
}

sealed trait CheckJVM { self: SourceKind =>
  def shouldValidate: Boolean = true
  def validate(options: SourceFile.Options) = {
    import CheckJVM.versionPattern
    options.data.get("jvm") match {
      case None => true // nothing to check
      case Some(value) => value.getOrElse("") match {
        case versionPattern(raw) => Properties.isJavaAtLeast(raw.toInt)
        case value => throw new IllegalArgumentException(s"Invalid JVM version: $value")
      }
    }
  }

}

object CheckJVM {
  val versionPattern: scala.util.matching.Regex = raw"(\d+)\+".r
}

object SourceKind {

  case object Scala     extends SourceKind(".scala") with PermitByName with CheckJVM
  case object ScalaFail extends SourceKind("_fail.scala") with PermitByName with AlwaysValid
  case object ScalaPre  extends SourceKind("_pre.scala") with PermitByName with AlwaysValid
  case object Check     extends SourceKind(".check") with PermitByName with AlwaysValid
  case object SkipCheck extends SourceKind(".skipcheck") with PermitByName with AlwaysValid
  case object Java      extends SourceKind(".java") with PermitByName with CheckJVM
  case object TastyFile extends SourceKind(".tasty") with PermitByName with AlwaysValid

  final case class ExactFiles(names: String*) extends SourceKind("") with AlwaysValid {
    override def permits(file: String) = names.contains(file)
  }

  def allowByKind(kinds: Set[SourceKind], paths: String*): Seq[String] = {
    if (kinds.isEmpty) Nil // no kinds, so allow nothing
    else {
      val bigPermit = kinds.foldLeft((_: SourceFile) => false) { (permits, kind) =>
        file =>
          kind.permits(file.path) && (!kind.shouldValidate || kind.validate(file.options)) || permits(file)
      }
      paths.view.map(new SourceFile(_)).filter(bigPermit).map(_.path).toSeq
    }
  }
}
