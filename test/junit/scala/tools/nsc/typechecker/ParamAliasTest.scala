package scala.tools.nsc.typechecker

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Global
import scala.tools.nsc.classpath.{AggregateClassPath, VirtualDirectoryClassPath}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class ParamAliasTest extends BytecodeTesting {

  @Test
  def checkAliasWorksWhenSubclassesAreTypecheckedFirst(): Unit = {
    def test(code: List[String], check: List[(String, String)], expected: List[String]): Unit = {
      val compiler1 = BytecodeTesting.newCompiler(extraArgs = compilerArgs)
      val r = new compiler1.global.Run
      r.compileSources(code.map(compiler1.global.newSourceFile(_)))
      Predef.assert(!compiler1.global.reporter.hasErrors, compiler1.global.reporter.asInstanceOf[StoreReporter].infos)
      def aliasNames(g: Global) = {
        check.map {
          case (clsName, paramName) =>
            val cls = g.rootMirror.getRequiredClass(clsName)
            val field = g.exitingPickler(cls.info.decl(g.TermName(paramName)).suchThat(_.isParamAccessor).accessed)
            assert(field.exists, (clsName, paramName, cls.info))
            val alias = field.alias
            s"${field.fullName} stored in ${alias.fullName}"
        }
      }
      val aliasInfoAfterCompilation = aliasNames(compiler1.global)
      val compiler2 = BytecodeTesting.newCompiler(extraArgs = compilerArgs)
      val out = compiler1.global.settings.outputDirs.getSingleOutput.get.asInstanceOf[VirtualDirectory]
      compiler2.global.platform.classPath
      compiler2.global.platform.currentClassPath = Some(AggregateClassPath(new VirtualDirectoryClassPath(out) :: compiler2.global.platform.currentClassPath.get :: Nil))
      val r2 = new compiler2.global.Run
      val aliasInfoUnpickled = aliasNames(compiler2.global)
      Assert.assertEquals(expected.sorted, aliasInfoAfterCompilation.sorted)
      Assert.assertEquals(expected.sorted, aliasInfoUnpickled.sorted)
    }

    {
      val code = List("package p1; class A(val a: Int) extends B(a)", "package p1; class B(b: Int) extends C(b)", "package p1; class C(val c: Int)")
      val check = List("p1.A" -> "a")
      val expected = List("p1.A.a stored in p1.C.c")
      test(code, check, expected)
      test(code.reverse, check, expected)
    }

    {
      val code = List("package p1; class A(val a: Int) extends B(a)", "package p1; class B(val b: Int) extends C(b)", "package p1; class C(val c: Int)")
      val check = List("p1.A" -> "a", "p1.B" -> "b")
      val expected = List("p1.A.a stored in p1.C.c", "p1.B.b stored in p1.C.c")
      test(code, check, expected)
      test(code.reverse, check, expected)
    }
  }
}
