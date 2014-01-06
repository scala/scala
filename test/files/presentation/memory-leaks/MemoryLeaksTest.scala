import java.io.PrintWriter
import java.io.FileOutputStream
import java.util.Calendar

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.interactive
import scala.tools.nsc.interactive.tests._
import scala.tools.nsc.io._
import scala.tools.nsc.doc

/** This test runs the presentation compiler on the Scala compiler project itself and records memory consumption.
 *
 * The test scenario is to open Typers, Trees and Types, then repeatedly add and remove one character
 * in Typers.scala. Each step causes the parser, namer, and type checker to run.
 *
 * At each step we record the memory usage after the GC has run. At the end of the test,
 * simple linear regression is used to compute the straight line that best fits the
 * curve, and if the slope is higher than 1 (meaning a leak of 1MB/run), we fail the test.
 *
 * The Scala compiler sources are assumed to be under 'basedir/src/compiler'.
 *
 * The individual data points are saved under 'usedMem-<date>.txt', under the test project
 * directory. Use the cool graph-it.R (https://github.com/scala-ide/scala-ide/blob/master/org.scala-ide.sdt.core.tests/graph-it.R)
 *  script to see the memory curve for the given test run.
 */
object Test extends InteractiveTest {
  final val mega = 1024 * 1024

  import interactive.Global
  trait InteractiveScaladocAnalyzer extends interactive.InteractiveAnalyzer with doc.ScaladocAnalyzer {
    val global : Global
    override def newTyper(context: Context) = new Typer(context) with InteractiveTyper with ScaladocTyper {
      override def canAdaptConstantTypeToLiteral = false
    }
  }

  private class ScaladocEnabledGlobal extends Global(settings, compilerReporter) {
    override lazy val analyzer = new {
      val global: ScaladocEnabledGlobal.this.type = ScaladocEnabledGlobal.this
    } with InteractiveScaladocAnalyzer
  }

  override def createGlobal: Global = new ScaladocEnabledGlobal

  override def execute(): Unit = memoryConsumptionTest()

  def batchSource(name: String) =
    new BatchSourceFile(AbstractFile.getFile(name))

  def memoryConsumptionTest() {
    val N = 50
    val filename = "usedmem-%tF.txt".format(Calendar.getInstance.getTime)

    val typerUnit = AbstractFile.getFile(baseDir.parent.parent.parent.parent / "src/compiler/scala/tools/nsc/typechecker/Typers.scala")
    val typesUnit = AbstractFile.getFile(baseDir.parent.parent.parent.parent / "src/reflect/scala/reflect/internal/Types.scala")
    val treesUnit = AbstractFile.getFile(baseDir.parent.parent.parent.parent / "src/reflect/scala/reflect/internal/Trees.scala")

    askReload(Seq(new BatchSourceFile(typerUnit), new BatchSourceFile(typesUnit), new BatchSourceFile(treesUnit)))
    typeCheckWith(treesUnit, new String(treesUnit.toCharArray))
    typeCheckWith(typesUnit, new String(typesUnit.toCharArray))

    val originalTyper = new String(typerUnit.toCharArray)

    val (prefix, postfix) = originalTyper.splitAt(originalTyper.indexOf("import global._"))
    val changedTyper = prefix + " a\n " + postfix

    val usedMem = for (i <- 1 to N) yield {
      val src = if (i % 2 == 0) originalTyper else changedTyper

      val usedMem = withGC {
        typeCheckWith(typerUnit, src)
      }

      usedMem / mega // report size in MB
    }

    //dumpDataToFile(filename, usedMem)
    // drop the first two measurements, since the compiler needs some memory when initializing
    val (a, b) = linearModel((3L to N).toSeq, usedMem.drop(2))
    //println("LinearModel: constant: %.4f\tslope:%.4f".format(a, b))

    if (b > 1.0)
      println("Rate of memory consumption is alarming! %.4f MB/run".format(b))
    else
      println("No leaks detected.")
  }

  private def typeCheckWith(file: AbstractFile, src: String) = {
    val sourceFile = new BatchSourceFile(file, src.toCharArray)
    askReload(Seq(sourceFile))
    askLoadedTyped(sourceFile).get // block until it's here
  }

  private def dumpDataToFile(filename: String, usedMem: Seq[Long]) {
    val outputFile = new PrintWriter(new FileOutputStream(filename))
    outputFile.println("\tusedMem")
    for ((dataPoint, i) <- usedMem.zipWithIndex) {
      outputFile.println("%d\t%d".format(i, dataPoint))
    }
    outputFile.close()
  }


  /** Return the linear model of these values, (a, b). First value is the constant factor,
   *  second value is the slope, i.e. `y = a + bx`
   *
   *  The linear model of a set of points is a straight line that minimizes the square distance
   *  between the each point and the line.
   *
   *  See: http://en.wikipedia.org/wiki/Simple_linear_regression
   */
  def linearModel(xs: Seq[Long], ys: Seq[Long]): (Double, Double) = {
    require(xs.length == ys.length)

    def mean(v: Seq[Long]): Double = v.sum.toDouble / v.length

    val meanXs = mean(xs)
    val meanYs = mean(ys)

    val beta = (mean((xs, ys).zipped.map(_ * _)) - meanXs * meanYs) / (mean(xs.map(x => x * x)) - meanXs * meanXs)
    val alfa = meanYs - beta * meanXs

    (alfa, beta)
  }

  /** Run the given closure and return the amount of used memory at the end of its execution.
   *
   *  Runs the GC before and after the execution of `f'.
   */
  def withGC(f: => Unit): Long = {
    val r = Runtime.getRuntime
    System.gc()

    f;

    System.gc()

    r.totalMemory() - r.freeMemory()
  }

}
