package scala.tools.nsc.symtab.classfile

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Global
import scala.tools.nsc.classpath.{AggregateClassPath, VirtualDirectoryClassPath}
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class PicklerTest extends BytecodeTesting {
  @Test
  def pickleUnpicklePreserveDeclOrder(): Unit = {
    assertStableDecls("package p1; trait C { def x: T; def y: Int; class T }", "p1.C")
    assertStableDecls("package p1; class D; object D { def x: T = null; def y: Int = 0; class T }", "p1.D")
  }

  def assertStableDecls(source: String, name: String): Unit = {
    val compiler1 = BytecodeTesting.newCompiler(extraArgs = compilerArgs)
    val r = new compiler1.global.Run
    r.compileSources(compiler1.global.newSourceFile(source) :: Nil)
    val compiler2 = BytecodeTesting.newCompiler(extraArgs = compilerArgs)
    val out = compiler1.global.settings.outputDirs.getSingleOutput.get.asInstanceOf[VirtualDirectory]
    def showDecls(global: Global): Seq[String] = global.exitingPickler {
      val classSym = global.rootMirror.getClassIfDefined(name)
      val moduleSym = global.rootMirror.getModuleIfDefined(name).moduleClass
      val syms = List(classSym, moduleSym).filter(sym => sym.exists)
      Assert.assertTrue(syms.nonEmpty)
      syms.flatMap(sym => sym.name.toString :: sym.info.decls.toList.map(decl => global.definitions.fullyInitializeSymbol(decl).defString))
    }
    val decls1 = showDecls(compiler1.global)
    compiler2.global.classPath
    compiler2.global.platform.currentClassPath = Some(AggregateClassPath(new VirtualDirectoryClassPath(out) :: compiler2.global.platform.currentClassPath.get :: Nil))
    new compiler2.global.Run
    val decls2 = showDecls(compiler2.global)
    Assert.assertEquals(decls1, decls2)
  }
}
