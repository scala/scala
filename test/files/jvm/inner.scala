//############################################################################
// Test Java interaction with scala inner classes
//############################################################################

import java.io.{BufferedReader, File, FileWriter, InputStreamReader}

class A {
  val abc = "A.abc"

  protected class B(x: Int, y: String) {
    println(abc); println(x)
    println(y)
  }

  trait Itf {
    def method1(x: Int): Int
    
    trait Itf2 extends Itf {
      def method2: Unit
    }
  }

  trait PlainTrait {
    def method1(x: Int): Int
  }

  class Impl(a: Int) extends Itf {
    def method1(x: Int) = {
      println(x)
      println(a)
      x + a
    }
  }

  val impl = new Impl(0)

  class Impl2 extends Impl(1) with impl.Itf2 {
    def method2 = {
      println(abc)
    }
  }

  def newImpl: Itf = new Impl(1)
  def newImpl2: Itf#Itf2 = new Impl2

  class Outer1(arg1: Int) {
    class Outer2(arg2: Int) {
      class Outer3(arg3: Int) {
        println("Outer3: " + arg1 + " " + arg2 + " " + arg3);
      }
    }
  }
}

object Scalatest {
  private val outputdir = System.getProperty("partest.output", "inner.obj")
  private val scalalib  = System.getProperty("partest.lib", "")
  private val classpath = outputdir + File.pathSeparator + scalalib
  private val javabin  = {
    val jhome = new File(System.getProperty("java.home"))
    if (jhome.getName == "jre")
      new File(jhome.getParent, "bin").getAbsolutePath
    else
      new File(jhome, "bin").getAbsolutePath
  }
  private val javacmd   = javabin + File.separator + "java"
  private val javac     = javabin + File.separator + "javac"

  def javac(src: String, fname: String) {
    val tmpfilename = outputdir + File.separator + fname
    val tmpfile = new FileWriter(tmpfilename)
    tmpfile.write(src)
    tmpfile.close
    exec(javac + " -d " + outputdir + " -classpath " + classpath + " " + tmpfilename)
  }

  def java(cname: String) =
    exec(javacmd + " -cp " + classpath + " " + cname)

  /** Execute cmd, wait for the process to end and pipe it's output to stdout */
  private def exec(cmd: String) {
    val proc = Runtime.getRuntime().exec(cmd)
    val inp = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val errp = new BufferedReader(new InputStreamReader(proc.getErrorStream))
    proc.waitFor()
    while (inp.ready) println(inp.readLine())
    while (errp.ready) println(errp.readLine())
  }
}

object Test {
  def main(args: Array[String]) {
    val javaInteraction = """
public class JavaInteraction {
    public static void main(String[] args) {
        A a = new A();
        A.B b = a.new B(1, "Hello");

        A.Itf itf = a.newImpl();
        itf.method1(1);

        A.Itf.Itf2 itf2 = a.newImpl2();
        itf2.method2();

        A.Outer1 o1 = a.new Outer1(1);
        A.Outer1.Outer2 o2 = o1.new Outer2(2);
        A.Outer1.Outer2.Outer3 or = o2.new Outer3(3);
    }
}
"""
    Scalatest.javac(javaInteraction, "JavaInteraction.java")
    Scalatest.java("JavaInteraction")

    val accessingScala = """
public class AccessingScala {
    public static void main(String[] args) {
        A a = new A();
        System.out.println(a.abc());
    }
}
"""
    Scalatest.javac(accessingScala, "AccessingScala.java")
    Scalatest.java("AccessingScala")
  }
}

//############################################################################
