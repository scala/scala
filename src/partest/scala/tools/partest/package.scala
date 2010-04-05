/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools

import scala.util.control.Exception.catching
import nsc.io.{ File, Path, Process, Directory }
import nsc.util.CommandLineSpec
import Process.runtime
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

  private[partest] def toArgs(line: String) = CommandLineSpec toArgs line
  private[partest] def fromArgs(args: List[String]) = CommandLineSpec fromArgs args

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

  /** Execute some code with a shutdown hook in place.  This is
   *  motivated by the desire not to leave the filesystem full of
   *  junk when someone ctrl-Cs a test run.
   */
  def withShutdownHook[T](hook: => Unit)(body: => T): Option[T] =
    /** Java doesn't like it if you keep adding and removing shutdown
     *  hooks after shutdown has begun, so we trap the failure.
     */
    catching(classOf[IllegalStateException]) opt {
      val t = new Thread() { override def run() = hook }
      runtime addShutdownHook t

      try body
      finally runtime removeShutdownHook t
    }

  /** Apply a function and return the passed value */
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }
}