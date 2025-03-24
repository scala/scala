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

// $Id$

package scala
package tools.nsc.settings

/** Represents a single Scala version in a manner that
 *  supports easy comparison and sorting.
 *
 *  A version can be `Specific`, `Maximal`, or `Minimal`.
 */
sealed abstract class ScalaVersion extends Ordered[ScalaVersion] {
  def unparse: String
  def versionString: String = unparse
}

/** A scala version that sorts higher than all actual versions. */
sealed abstract class MaximalScalaVersion extends ScalaVersion {
  final def compare(that: ScalaVersion): Int = that match {
    case _: MaximalScalaVersion => 0
    case _ => 1
  }
}

/** A scala version that sorts lower than all actual versions. */
sealed abstract class MinimalScalaVersion extends ScalaVersion {
  final def compare(that: ScalaVersion): Int = that match {
    case _: MinimalScalaVersion => 0
    case _ => -1
  }
}

/** If "no version" is specified, assume a maximal version, "the latest". */
case object NoScalaVersion extends MaximalScalaVersion {
  def unparse = "none"
}

/** A specific Scala version, not one of the magic min/max versions.
 *
 *  A SpecificScalaVersion may or may not be a released version.
 *  The `build` parameter specifies final, release candidate, milestone, and development builds.
 */
case class SpecificScalaVersion(major: Int, minor: Int, rev: Int, build: ScalaBuild) extends ScalaVersion {
  def unparse = s"${major}.${minor}.${rev}${build.unparse}"
  override def versionString = s"${major}.${minor}.${rev}"

  final def compare(that: ScalaVersion): Int =  that match {
    case SpecificScalaVersion(thatMajor, thatMinor, thatRev, thatBuild) =>
      // this could be done more cleanly by importing scala.math.Ordering.Implicits, but we have to do these
      // comparisons a lot so I'm using brute force direct style code
      if (major < thatMajor) -1
      else if (major > thatMajor) 1
      else if (minor < thatMinor) -1
      else if (minor > thatMinor) 1
      else if (rev < thatRev) -1
      else if (rev > thatRev) 1
      else build.compare(thatBuild)
    case _: MinimalScalaVersion => 1
    case _: MaximalScalaVersion => -1
  }
}

/** A Scala version that sorts lower than all actual versions.
 */
case object AnyScalaVersion extends MinimalScalaVersion {
  def unparse = "any"
}

/** Factory methods for producing ScalaVersions.
 */
object ScalaVersion {
  private val dot   = """\."""
  private val dash  = "-"
  private val vchar = """\d""" //"[^-+.]"
  private val vpat  = s"(?s)($vchar+)(?:$dot($vchar+)(?:$dot($vchar+))?)?(?:$dash(.+))?".r
  private val rcpat = """(?i)rc(\d*)""".r
  private val mspat = """(?i)m(\d*)""".r

  def apply(versionString: String, errorHandler: String => Unit): ScalaVersion = {
    def error() = errorHandler(
      s"Bad version (${versionString}) not major[.minor[.revision]][-suffix]"
    )

    def toInt(s: String) = s match {
      case null | "" => 0
      case _         => s.toInt
    }

    def toBuild(s: String) = s match {
      case null | "FINAL" => Final
      case rcpat(i)       => RC(toInt(i))
      case mspat(i)       => Milestone(toInt(i))
      case _ /* | "" */   => Development(s)
    }

    versionString match {
      case "none" | ""   => NoScalaVersion
      case "any"         => AnyScalaVersion
      case vpat(majorS, minorS, revS, buildS) =>
        SpecificScalaVersion(toInt(majorS), toInt(minorS), toInt(revS), toBuild(buildS))
      case _             => error(); AnyScalaVersion
    }
  }

  def apply(versionString: String): ScalaVersion =
      apply(versionString, msg => throw new NumberFormatException(msg))

  /**
   * The version of the compiler running now
   */
  val current = apply(util.Properties.versionNumberString)

  implicit class `not in Ordered`(private val v: ScalaVersion) extends AnyVal {
    def min(other: ScalaVersion): ScalaVersion = if (v <= other) v else other
    def max(other: ScalaVersion): ScalaVersion = if (v >= other) v else other
  }
}

/** Represents the data after the dash in major.minor.rev-build.
 *
 *  In order, Development, Final, RC, Milestone. The order is "newest to oldest".
 */
sealed abstract class ScalaBuild extends Ordered[ScalaBuild] {
  /** Return a version of this build information that can be parsed back into the same ScalaBuild.  */
  def unparse: String

  final def compare(that: ScalaBuild) = buildOrdering.compare(this, that)
}
private object buildOrdering extends Ordering[ScalaBuild] {
  override def compare(x: ScalaBuild, y: ScalaBuild): Int =
    x match {
      case Development(id) =>
        y match {
          // sorting by id is pragmatic but not meaningful, such as "cross" < "migration"
          case Development(thatId) => id.compare(thatId)
          case _ => 1 // otherwise, newer than official builds, which is incorrect on the "build timeline"
        }
      case Milestone(n) =>
        y match {
          case Milestone(thatN) => n - thatN // compare two milestones based on their milestone numbers
          case _ => -1 // a milestone is older than anything other than another milestone
        }
      case RC(n) =>
        y match {
          case RC(thatN) => n - thatN // compare two rcs based on their RC numbers
          case Milestone(_) => 1 // an rc is older than anything other than a milestone or another rc
          case _ => -1
        }
      case Final =>
        y match {
          case Final => 0 // a final is newer than anything other than a development build or another final
          case Development(_) => -1
          case _ => 1
        }
    }
}
/** A development, test, integration, snapshot or other "unofficial" build.
 */
case class Development(id: String) extends ScalaBuild {
  def unparse = s"-${id}"
}
/** A final final.
 */
case object Final extends ScalaBuild {
  def unparse = ""
}
/** A candidate for final release.
 */
case class RC(n: Int) extends ScalaBuild {
  def unparse = s"-RC${n}"
}
/** An intermediate release.
 */
case class Milestone(n: Int) extends ScalaBuild {
  def unparse = s"-M${n}"
}
