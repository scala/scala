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

package scala.tools.partest.nest

import scala.reflect.OptManifest

/** A general mechanism for defining how a command line argument
 *  (always a String) is transformed into an arbitrary type.  A few
 *  example instances are in the companion object, but in general
 *  either IntFromString will suffice or you'll want custom transformers.
 */
abstract class FromString[+T](implicit m: OptManifest[T]) extends PartialFunction[String, T] {
  def apply(s: String): T
  def isDefinedAt(s: String): Boolean = true
  def zero: T = apply("")

  def targetString: String = m.toString
}

object FromString {
  import scala.sys.process.Parser.tokenize
  import scala.tools.nsc.io.Directory
  // We need this because we clash with the String => Path implicits.
  private def toDir(s: String)  = new Directory(new java.io.File(s))

  /** Path related stringifiers.
   */
  val ExistingDir: FromString[Directory] = new FromString[Directory] {
    override def isDefinedAt(s: String) = toDir(s).isDirectory
    def apply(s: String): Directory =
      if (isDefinedAt(s)) toDir(s)
      else runAndExit(println("'%s' is not an existing directory." format s))
  }
  def ExistingDirRelativeTo(root: Directory) = new FromString[Directory] {
    private def resolve(s: String) = (toDir(s) toAbsoluteWithRoot root).toDirectory
    override def isDefinedAt(s: String) = resolve(s).isDirectory
    def apply(s: String): Directory =
      if (isDefinedAt(s)) resolve(s)
      else runAndExit(println("'%s' is not an existing directory." format resolve(s)))
  }

  /** Argument expander, i.e. turns single argument "foo bar baz" into argument
   *  list "foo", "bar", "baz".
   */
  val ArgumentsFromString: FromString[List[String]] = new FromString[List[String]] {
    def apply(s: String) = tokenize(s)
  }

  /** Identity.
   */
  implicit val StringFromString: FromString[String] = new FromString[String] {
    def apply(s: String): String = s
  }

  /** Implicit as the most likely to be useful as-is.
   */
  implicit val IntFromString: FromString[Int] = new FromString[Int] {
    override def isDefinedAt(s: String)   = s.toIntOption.isDefined
    def apply(s: String)                  = s.toIntOption.getOrElse(0)
  }
}
