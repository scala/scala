/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 */

package scala.tools

import java.io.{ FileNotFoundException, File => JFile }
import nsc.io.{ Path, Directory, File => SFile }
import scala.tools.util.PathResolver
import nsc.Properties.{ propOrElse, propOrNone, propOrEmpty }
import scala.sys.process.javaVmArguments
import java.util.concurrent.Callable

package partest {
  class TestState { }
  object TestState {
    val Ok      = new TestState
    val Fail    = new TestState
    val Timeout = new TestState
  }
}

package object partest {
  import nest.NestUI

  implicit private[partest] def temporaryPath2File(x: Path): JFile = x.jfile
  implicit private[partest] def temporaryFile2Path(x: JFile): Path = Path(x)

  implicit lazy val postfixOps = scala.language.postfixOps
  implicit lazy val implicitConversions = scala.language.implicitConversions

  def timed[T](body: => T): (T, Long) = {
    val t1 = System.currentTimeMillis
    val result = body
    val t2 = System.currentTimeMillis

    (result, t2 - t1)
  }

  def callable[T](body: => T): Callable[T] = new Callable[T] { override def call() = body }

  def file2String(f: JFile) =
    try SFile(f).slurp(scala.io.Codec.UTF8)
    catch { case _: FileNotFoundException => "" }

  def basename(name: String): String = Path(name).stripExtension

  def resultsToStatistics(results: Iterable[(_, TestState)]): (Int, Int) = {
    val (files, failures) = results map (_._2 == TestState.Ok) partition (_ == true)
    (files.size, failures.size)
  }

  def vmArgString = javaVmArguments.mkString(
    "Java VM started with arguments: '",
    " ",
    "'"
  )

  def allPropertiesString = {
    import scala.collection.JavaConversions._
    System.getProperties.toList.sorted map { case (k, v) => "%s -> %s\n".format(k, v) } mkString ""
  }

  def showAllJVMInfo() {
    NestUI.verbose(vmArgString)
    NestUI.verbose(allPropertiesString)
  }

  def isPartestDebug: Boolean =
    propOrEmpty("partest.debug") == "true"

  import scala.language.experimental.macros

  /**
   * `trace("".isEmpty)` will return `true` and as a side effect print the following to standard out.
   * {{{
   *   trace> "".isEmpty
   *   res: Boolean = true
   *
   * }}}
   *
   * An alternative to [[scala.tools.partest.ReplTest]] that avoids the inconvenience of embedding
   * test code in a string.
   */
  def trace[A](a: A) = macro traceImpl[A]

  import scala.reflect.macros.Context
  def traceImpl[A: c.WeakTypeTag](c: Context)(a: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    import definitions._

    // xeno.by: reify shouldn't be used explicitly before the final release of 2.10.0,
    // because this impairs reflection refactorings
    //
    // val exprCode = c.literal(show(a.tree))
    // val exprType = c.literal(show(a.actualType))
    // reify {
    //   println(s"trace> ${exprCode.splice}\nres: ${exprType.splice} = ${a.splice}\n")
    //   a.splice
    // }

    c.Expr(Block(
      List(Apply(
        Select(Ident(PredefModule), newTermName("println")),
        List(Apply(
          Select(Apply(
            Select(Ident(ScalaPackage), newTermName("StringContext")),
            List(
              Literal(Constant("trace> ")),
              Literal(Constant("\\nres: ")),
              Literal(Constant(" = ")),
              Literal(Constant("\\n")))),
          newTermName("s")),
          List(
            Literal(Constant(show(a.tree))),
            Literal(Constant(show(a.actualType))),
            a.tree))))),
      a.tree))
  }
}
