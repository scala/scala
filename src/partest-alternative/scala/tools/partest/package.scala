/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools

import nsc.io.{ File, Path, Process, Directory }
import java.nio.charset.CharacterCodingException

package object partest {
  /** The CharacterCodingExceptions are thrown at least on windows trying
   *  to read a file like script/utf-8.scala
   */
  private[partest] def safeSlurp(f: File) =
    try if (f.exists) f.slurp() else ""
    catch { case _: CharacterCodingException  => "" }

  private[partest] def safeLines(f: File)     = safeSlurp(f) split """\r\n|\r|\n""" toList
  private[partest] def safeArgs(f: File)      = toArgs(safeSlurp(f))
  private[partest] def isJava(f: Path)        = f.isFile && (f hasExtension "java")
  private[partest] def isScala(f: Path)       = f.isFile && (f hasExtension "scala")
  private[partest] def isJavaOrScala(f: Path) = isJava(f) || isScala(f)

  private[partest] def toArgs(line: String) = cmd toArgs line
  private[partest] def fromArgs(args: List[String]) = cmd fromArgs args

  /** Strings, argument lists, etc. */

  private[partest] def fromAnyArgs(args: List[Any]) = args mkString " " // separate to avoid accidents
  private[partest] def toStringTrunc(x: Any, max: Int = 240) = {
    val s = x.toString
    if (s.length < max) s
    else (s take max) + " [...]"
  }
  private[partest] def setProp(k: String, v: String) = scala.util.Properties.setProp(k, v)

  /** Pretty self explanatory. */
  def printAndExit(msg: String): Unit = {
    println(msg)
    exit(1)
  }

  /** Apply a function and return the passed value */
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }
}