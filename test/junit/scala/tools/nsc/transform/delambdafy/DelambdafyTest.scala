package scala.tools.nsc.transform.delambdafy

import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.io.Path.jfile2path
import scala.tools.nsc.io.AbstractFile
import scala.tools.testing.BytecodeTesting._
import scala.tools.testing.TempDir

@RunWith(classOf[JUnit4])
class DelambdafyTest {
  def compileToMultipleOutputWithDelamdbafyMethod(): List[(String, Array[Byte])] = {
    val codeForMultiOutput = """
object Delambdafy {
  type -->[D, I] = PartialFunction[D, I]

  def main(args: Array[String]): Unit = {
    val result = List(1, 2, 4).map { a =>
      val list = List("1", "2", "3").map { _ + "test" }
      list.find { _ == a.toString + "test" }
    }
    println(result)
    lazy val _foo = foo(result) {
      case x :: xs if x isDefined => x.get.length
      case _ => 0
    }
    println(_foo)
    lazy val bar: Int => Int = {
      case 2 => 23
      case _ =>
        val v = List(1).map { _ + 42 }.head
        v + 31
    }
    bar(3)
    lazy val _baz = baz {
      case 1 =>
        val local = List(1).map(_ + 1)
        local.head
    }
  }

  def baz[T](f: Any --> Any): Any => Any = f

  def foo(b: List[Option[String]])(a: List[Option[String]] => Int): Int = a(b)
}
"""
    val srcFile = makeSourceFile(codeForMultiOutput, "delambdafyTest.scala")
    val outDir = AbstractFile.getDirectory(TempDir.createTempDir())
    val outDirPath = outDir.canonicalPath
    val extraArgs = "-Ydelambdafy:method"
    val argsWithOutDir = extraArgs + s" -d $outDirPath -cp $outDirPath"
    val compiler = newCompilerWithoutVirtualOutdir(extraArgs = argsWithOutDir)
    compiler.global.settings.outputDirs.add(srcFile.file, outDir)

    new compiler.global.Run().compileSources(List(srcFile))

    val classfiles = getGeneratedClassfiles(outDir)
    outDir.delete()
    classfiles
  }

  @Test
  def shouldFindOutputFoldersForAllPromotedLambdasAsMethod(): Unit = {
    val actual = compileToMultipleOutputWithDelamdbafyMethod()

    assertTrue(actual.length > 0)
  }
}
