
import scala.tools.partest.StoreReporterDirectTest
import scala.tools.nsc._
import scala.util.chaining._

object Test extends StoreReporterDirectTest {
  var round = 0
  def code1 = "package p { trait T }"
  def code2 =
    sm"""|package p {
         |  object T
         |  class C extends T
         |}"""
  def code =
    if (round > 0) code2
    else code1.tap(_ => round += 1)
  // intercept the settings created for compile()
  override def newSettings(args: List[String]) = {
    val outDir = testOutput.parent / testOutput / s"test-output-$round"
    def prevOutDir = testOutput.parent / testOutput / s"test-output-${round - 1}"
    outDir.createDirectory()
    // put the previous round on the class path
    val ss =
      if (round > 0) super.newSettings("-cp" :: prevOutDir.path :: args)
      else super.newSettings(args)
    ss.outputDirs.add(srcDir = testPath.parent.path, outDir = outDir.path)
    ss
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
