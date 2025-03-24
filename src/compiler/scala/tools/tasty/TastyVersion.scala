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

package scala.tools.tasty

// sourced from: https://github.com/scala/scala3/blob/release-3.4.0/tasty/src/dotty/tools/tasty/TastyVersion.scala

case class TastyVersion private(major: Int, minor: Int, experimental: Int) {
  def isExperimental: Boolean = experimental > 0

  def nextStable: TastyVersion = copy(experimental = 0)

  def minStable: TastyVersion = copy(minor = 0, experimental = 0)

  def show: String = {
    val suffix = if (isExperimental) s"-experimental-$experimental" else ""
    s"$major.$minor$suffix"
  }

  def kind: String =
    if (isExperimental) "experimental TASTy" else "TASTy"

  def validRange: String = {
    val min = TastyVersion(major, 0, 0)
    val max = if (experimental == 0) this else TastyVersion(major, minor - 1, 0)
    val extra = Option.when(experimental > 0)(this)
    s"stable TASTy from ${min.show} to ${max.show}${extra.fold("")(e => s", or exactly ${e.show}")}"
  }
}

object TastyVersion {

  private val cache: java.util.concurrent.ConcurrentHashMap[TastyVersion, TastyVersion] =
    new java.util.concurrent.ConcurrentHashMap()

  def apply(major: Int, minor: Int, experimental: Int): TastyVersion = {
    val version = new TastyVersion(major, minor, experimental)
    val cachedVersion = cache.putIfAbsent(version, version)
    if (cachedVersion == null) version else cachedVersion
  }
}
