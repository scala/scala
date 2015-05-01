import java.io._

object Scalatest {
  val outputdir = System.getProperty("partest.output", "inner.obj")
  val scalalib  = System.getProperty("partest.lib", "")
  val classpath = outputdir + File.pathSeparator + scalalib
  val javacmd   = System.getProperty("javacmd", "java")
  val javac     = System.getProperty("javaccmd", "javac")

  def javac(src: String, opts: String, fname: String) {
    val tmpfilename = outputdir + File.separator + fname
    val tmpfile = new FileWriter(tmpfilename)
    tmpfile.write(src)
    tmpfile.close
    exec(javac + " -d " + outputdir + " -classpath " + classpath + " " + opts + tmpfilename)
  }

  def java(cname: String) =
    exec(javacmd + " -cp " + classpath + " " + cname)

  class Slurp(in: BufferedReader) extends Thread("slurper") {
    var done = false
    override def run() {
      while (!done)  if (in.ready) println(in.readLine())
    }
  }

  def slurp(in: BufferedReader): Slurp = {
    val s = new Slurp(in)
    s.start()
    s
  }
    

  /** Execute cmd, wait for the process to end and pipe its output to stdout */
  def exec(cmd: String) {
    val proc = Runtime.getRuntime().exec(cmd)
    val inp = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val errp = new BufferedReader(new InputStreamReader(proc.getErrorStream))
    val t1 = slurp(inp)
    val t2 = slurp(errp)
    proc.waitFor()
    t1.done = true
    t2.done = true
    t1.join()
    t2.join()
  }
}

// Test correct java signatures for anonymous classes. Enclosing method attributes should
// allow javac to see the type parameters in foo. See #3249.

class A[U] { 
   def bar[B](x : => B) = x
   def foo[C](c : C) : C = bar(c)
}

object B { 
   def bar[B](x : => B) = x
   def foo[C](c : C) : C = {
     class InnerB(x: C)
     c
   }
}

class B {
  def foo {}
}

object Test {
  def main(args: Array[String]) {
    import Scalatest._
    exec("%s -Xprint -cp %s A".format(javac, classpath))
    exec("%s -Xprint -cp %s B".format(javac, classpath))
    exec("%s -Xprint -cp %s A$$anonfun$foo$1".format(javac, classpath))
    exec("%s -Xprint -cp %s scala.actors.Actor".format(javac, classpath))
  }
}
