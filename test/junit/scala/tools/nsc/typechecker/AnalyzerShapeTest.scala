package scala.tools.nsc.typechecker

import org.junit.Assert._
import org.junit.Test

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.StoreReporter

/** The shape that keeps the typechecker's calls to its analyzer `invokevirtual` and old clients linking. */
class AnalyzerShapeTest {
  @Test def partsAreSelfTypedWithTheClass(): Unit = {
    assertEquals(classOf[AbstractAnalyzer], classOf[Contexts#Context].getField("$outer").getType)
    assertEquals(classOf[AbstractAnalyzer], classOf[Typers#Typer].getMethod("scala$tools$nsc$typechecker$Typers$Typer$$$outer").getReturnType)
  }

  @Test def analyzerStaysAnInterfaceWithGlobal(): Unit = {
    assertTrue(classOf[Analyzer].isInterface)
    assertEquals(classOf[Global], classOf[Analyzer].getMethod("global").getReturnType)
  }

  @Test def globalsAnalyzerIsBoth(): Unit = {
    val settings = new Settings
    val analyzer: AnyRef = new Global(settings, new StoreReporter(settings)).analyzer
    assertEquals(classOf[AbstractAnalyzer], analyzer.getClass.getSuperclass)
    assertTrue(analyzer.isInstanceOf[Analyzer])
  }
}
