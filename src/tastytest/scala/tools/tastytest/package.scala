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

package scala.tools

package object tastytest {

  import scala.util.Try

  import Files.{pathSep, classpathSep}

  def printerrln(str: String): Unit = System.err.println(red(str))
  def printwarnln(str: String): Unit = System.err.println(yellow(str))
  def printsuccessln(str: String): Unit = System.err.println(green(str))

  implicit final class ZipOps[T](val t: Try[T]) extends AnyVal {
    @inline final def <*>[U](u: Try[U]): Try[(T, U)] = for {
      x <- t
      y <- u
    } yield (x, y)
  }

  def cyan(str: String): String = Console.CYAN + str + Console.RESET
  def yellow(str: String): String = Console.YELLOW + str + Console.RESET
  def red(str: String): String = Console.RED + str + Console.RESET
  def green(str: String): String = Console.GREEN + str + Console.RESET

  implicit final class PathOps(val s: String) extends AnyVal {
    @inline final def / (part: String): String = path(s, part)
    @inline final def / (parts: Seq[String]): String = path(s, parts:_*)

    /** replace '.' by '/'. */
    @inline final def toBinaryName : String = s.replace(raw"\.", pathSep) + "/"
  }

  private def path(part: String, parts: String*): String = (part +: parts).mkString(pathSep)
  def classpath(path: String, paths: String*): String = (path +: paths).mkString(classpathSep)

  implicit final class OptionOps[A](val opt: Option[A]) extends AnyVal {
    @inline final def failOnEmpty(ifEmpty: => Throwable): Try[A] = opt.toRight(ifEmpty).toTry
  }

}
