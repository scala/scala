
import scala.reflect.io.AbstractFile
import scala.tools.nsc._
import scala.tools.partest.StoreReporterDirectTest
import scala.util.chaining._

object Test extends StoreReporterDirectTest {
  var round = 0
  val rounds = List(
    "package p { trait T }",
    sm"""|package p {
         |  object T
         |  class C extends T
         |}""",
  )
  def code = rounds(round).tap(_ => round += 1)
  // intercept the settings created for compile()
  override def newSettings(args: List[String]) = {
    val outDir = testOutput / s"test-output-$round"
    def prevOutDir = testOutput / s"test-output-${round - 1}"
    outDir.createDirectory(force = true)
    val af = AbstractFile.getDirectory(outDir)
    // put the previous round on the class path
    val args1 = if (round > 0) "-cp" :: prevOutDir.path :: args else args
    super.newSettings(args1)
      .tap(_.outputDirs.setSingleOutput(af))
  }
  // avoid setting -d
  override def newCompiler(args: String*): Global = {
    val settings = newSettings(tokenize(extraSettings) ++ args.toList)
    newCompiler(settings)
  }
  // report helpful message when output dir is programmatically set
  def show() = {
    assert(compile())
    assert(!compile())
    filteredInfos.foreach(println)
  }
}
