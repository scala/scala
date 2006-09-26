//############################################################################
// Test Java interaction with scala inner classes
//############################################################################
// $Id$

import java.io.{BufferedReader, File, FileWriter, InputStreamReader}

class A {
  val abc = "A.abc"

  protected class B(x: Int, y: String) {
    Console.println(abc); Console.println(x)
    Console.println(y)
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
      Console.println(x)
      Console.println(a)
      x + a
    }
  }

  class Impl2 extends Impl(1) with Itf#Itf2 {
    def method2 = {
      Console.println(abc)
    }
  }

  def newImpl: Itf = new Impl(1)
  def newImpl2: Itf#Itf2 = new Impl2

  class Outer1(arg1: Int) {
    class Outer2(arg2: Int) {
      class Outer3(arg3: Int) {
        Console.println("Outer3: " + arg1 + " " + arg2 + " " + arg3);
      }
    }
  }
}

object Scalatest {
  val outputdir = System.getProperty("scalatest.output", "inner-jvm.obj")
  val scalalib  = System.getProperty("scalatest.lib", "")
  val classpath = outputdir + File.pathSeparator + scalalib

  def javac(src: String) = {
    val tmpfilename = outputdir + File.separator + "tmpJavaInterraction.java"
    val tmpfile = new FileWriter(tmpfilename)
    tmpfile.write(src)
    tmpfile.close
    exec("javac -d " + outputdir + " -classpath " + classpath + " " + tmpfilename)
  }

  /** Execute cmd, wait for the process to end and pipe it's output to stdout */
  def exec(cmd: String) = {
    val proc = Runtime.getRuntime().exec(cmd);
    val inp = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val errp = new BufferedReader(new InputStreamReader(proc.getErrorStream))
    proc.waitFor()
    while (inp.ready) Console.println(inp.readLine())
    while (errp.ready) Console.println(errp.readLine())
  }
}


object Test {
  val program = """
public class tmpJavaInterraction {

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
  def main(args: Array[String]): Unit = {
    Scalatest.javac(program)
    Scalatest.exec("java -cp " + Scalatest.classpath + " tmpJavaInterraction")
  }
}

//############################################################################
