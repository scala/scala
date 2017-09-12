package scala.tools.nsc
package backend.jvm
package opt

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.JavaConverters._
import scala.tools.nsc.backend.jvm.BTypes.MethodInlineInfo
import scala.tools.nsc.backend.jvm.BackendReporting._
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class InlineInfoTest extends BytecodeTesting {
  import compiler._
  import global.genBCode.{bTypes, postProcessor}

  override def compilerArgs = "-opt:l:inline -opt-inline-from:**"

  compiler.keepPerRunCachesAfterRun(List(
    bTypes.classBTypeCacheFromSymbol,
    bTypes.classBTypeCacheFromClassfile,
    postProcessor.byteCodeRepository.compilingClasses,
    postProcessor.byteCodeRepository.parsedClasses))

  @Test
  def inlineInfosFromSymbolAndAttribute(): Unit = {
    val code =
      """trait T {
        |  @inline def f: Int
        |  @noinline final def g = 0
        |}
        |trait U { self: T =>
        |  @inline def f = 0
        |  final def h = 0
        |  final class K {
        |    @inline def i = 0
        |  }
        |}
        |sealed trait V {
        |  @inline def j = 0
        |}
        |class C extends T with U
      """.stripMargin
    val classes = compileClasses(code)

    val fromSyms = classes.map(c => global.genBCode.bTypes.cachedClassBType(c.name).get.info.get.inlineInfo)

    val fromAttrs = classes.map(c => {
      assert(c.attrs.asScala.exists(_.isInstanceOf[InlineInfoAttribute]), c.attrs)
      global.genBCode.postProcessor.bTypesFromClassfile.inlineInfoFromClassfile(c)
    })

    assert(fromSyms == fromAttrs)
  }

  @Test // scala-dev#20
  def javaStaticMethodsInlineInfoInMixedCompilation(): Unit = {
    val jCode =
      """public class A {
        |  public static final int bar() { return 100; }
        |  public final int baz() { return 100; }
        |}
      """.stripMargin
    compileClasses("class C { new A }", javaCode = List((jCode, "A.java")))
    val info = global.genBCode.bTypes.cachedClassBType("A").get.info.get.inlineInfo
    assertEquals(info.methodInfos, Map(
      "bar()I"    -> MethodInlineInfo(true,false,false),
      "<init>()V" -> MethodInlineInfo(false,false,false),
      "baz()I"    -> MethodInlineInfo(true,false,false)))
  }

  @Test
  def sd402(): Unit = {
    val jCode =
      """package java.nio.file;
        |public interface WatchEvent<T> {
        |  public static interface Kind<T> {
        |    static default String HAI() { return ""; }
        |  }
        |}
        |
      """.stripMargin
    compileClasses("class C { def t: java.nio.file.WatchEvent.Kind[String] = null }", javaCode = List((jCode, "WatchEvent.java")))
    // before the fix of scala-dev#402, the companion of the nested class `Kind` (containing the static method) was taken from
    // the classpath (classfile WatchEvent$Kind.class) instead of the actual companion from the source, so the static method was missing.
    val info = global.genBCode.bTypes.cachedClassBType("java/nio/file/WatchEvent$Kind").get.info.get.inlineInfo
    assertEquals(info.methodInfos, Map(
      "HAI()Ljava/lang/String;" -> MethodInlineInfo(true,false,false),
      "<init>()V"               -> MethodInlineInfo(false,false,false)))
  }
}
