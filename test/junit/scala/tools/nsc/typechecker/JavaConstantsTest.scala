package scala.tools.nsc.typechecker

import org.junit.Assert.assertEquals
import org.junit.Test

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import javax.tools.ToolProvider

import scala.reflect.internal.util.{BatchSourceFile, SourceFile}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{FileUtils, Global, Settings}

/** Fields of Java classes parsed from source have the same (constant) types as when they are read from
 *  classfiles emitted by javac, where constant variables have a `ConstantValue` attribute.
 */
class JavaConstantsTest {
  val J = """
    |public class J {
    |  public static final int I1 = 4 * 1024;
    |  public static final int I2 = I1 + 1;
    |  public static final int I3 = J.I1 << 2;
    |  public static final long L1 = 1L << 40, L2 = -I1;
    |  public static final int MIN = -2147483648;
    |  public static final long LMIN = -9223372036854775808L;
    |  public static final byte B1 = 10 + 20;
    |  public static final byte B2 = (byte) 300;
    |  public static final short S1 = (short) -70000;
    |  public static final char C1 = 'a' + 1;
    |  public static final char C2 = (char) (C1 + 1);
    |  public static final int C3 = 'a' + 'b';
    |  public static final int C4 = -'a';
    |  public static final float F1 = 1.0f / 3;
    |  public static final float F2 = 1.00000017881393432617187499f, F3 = -1.00000017881393432617187499f * 1;
    |  public static final double D1 = 1 / 2;
    |  public static final double D2 = 1.0 / 0;
    |  public static final double D3 = 5.5 % 2;
    |  public static final int MOD = -7 % 3, DIV = -7 / 2;
    |  public static final int SHR = -16 >> 2, USHR = -16 >>> 28, SHL = 1 << 33L;
    |  public static final long LUSHR = -1L >>> 63;
    |  public static final boolean Z1 = I1 > 100 && !false || I2 == 3;
    |  public static final boolean Z2 = true ^ true, Z3 = 1.0 == 1.0f, Z4 = Double.NaN != Double.NaN;
    |  public static final int TERN = I1 > 0 ? 1 : 2;
    |  public static final char TERNC = true ? 'x' : 0;
    |  public static final String S1S = "a" + 1 + 'c' + 1.5f + 2.5 + true + 10L + I1 + 1e10 + Float.MIN_VALUE;
    |  public static final String S2S = "x" + (char) 65 + (1 + 2) + (String) "y" + (java.lang.String) "z";
    |  public static final String S3S = "" + ("a" == "a") + TERNC;
    |  public static final java.lang.String S4S = "a";
    |  public static final int NEG = ~I1, PLUS = +C1;
    |  public static final int CYC1 = J.CYC2 + 0, CYC2 = J.CYC1 + 0;
    |  public static final int CYC4 = J.CYC3 + 0, CYC3 = J.CYC4 + 0;
    |  public static final int SIZE = 4;
    |  public static int SIZE() { return 0; }
    |  public static final int BYTES = SIZE * 2, BYTES2 = J.SIZE * 2;
    |  public static final int INTMAX = Integer.MAX_VALUE + 1;
    |  public static final int OVF = Integer.MIN_VALUE / -1, OVF2 = Integer.MAX_VALUE * 2, DIVZ = 1 / 0;
    |  public static final long LOVF = Long.MAX_VALUE * 2, LSHL = 1L << 65, BSHL = (byte) 1 << 33L;
    |  public static final short SNEG = -(short) 1;
    |  public static final int PAREN = (I1) - 1, CAST = (int) 3.9 + (int) -3.9, CASTL = (int) 1e20;
    |  public static final float FCAST = (float) 1e40;
    |  public static final long LONGCHAR = 'a' * 1000000000L;
    |  public static final int NOTCONST = Integer.parseInt("1");
    |  public static final int NOTCONST2 = new int[]{1, 2}.length, AFTER = (3 + 4);
    |  public static final int NOTCONST3 = (Integer.valueOf(1)) + 1, AFTER2 = 5;
    |  public static final Integer BOXED = 1 + 1;
    |  public static final Object OBJ = "a" + "b";
    |  public final int INSTANCE = 1 + 1;
    |  public static int NONFINAL = 1 + 1;
    |}
    |interface I { int IC = 7 * 6; String IS = "i" + IC; }
    |class K implements I {
    |  static final int K1 = IC + J.I1;
    |  static final int K2 = I.IC + 1;
    |  static final String K3 = IS + K1;
    |}
    |""".stripMargin

  def newGlobal(classpath: String): Global = {
    val settings = new Settings
    settings.usejavacp.value = true
    settings.classpath.value = classpath
    settings.outputDirs.setSingleOutput(new VirtualDirectory("out", None))
    new Global(settings, new StoreReporter(settings))
  }

  def fieldTypes(g: Global, javaSources: List[SourceFile]): List[String] = {
    import g._, rootMirror.EmptyPackageClass
    val run = new Run
    run.compileSources(javaSources)
    assert(!reporter.hasErrors, reporter.asInstanceOf[StoreReporter].infos.mkString("\n"))
    exitingTyper {
      for {
        cls  <- List("J", "I", "K")
        sym  <- List(EmptyPackageClass.info.decl(TypeName(cls)), EmptyPackageClass.info.decl(TermName(cls)).moduleClass)
        decl <- sym.info.decls.toList if decl.isValue && !decl.isMethod
      } yield s"$cls.${decl.name.decoded}: ${decl.info}"
    }.sorted
  }

  @Test def constantTypesFromSourceMatchClassfile(): Unit = {
    val out = Files.createTempDirectory("javac-out")
    try {
      val src = Files.write(out.resolve("J.java"), J.getBytes(UTF_8))
      assertEquals(0, ToolProvider.getSystemJavaCompiler.run(null, null, null, "-d", out.toString, src.toString))
      Files.delete(src)

      val expected = fieldTypes(newGlobal(out.toString), Nil)
      assert(expected.contains("J.I1: Int(4096)"), expected.mkString("\n"))
      val actual = fieldTypes(newGlobal(""), List(new BatchSourceFile("J.java", J)))
      assertEquals(expected.mkString("\n"), actual.mkString("\n"))
    } finally FileUtils.deleteRecursive(out)
  }
}
