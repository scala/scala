
import scala.tools.nsc.settings.MutableSettings

object Test {
  val tokens        = "" :: "-deprecation" :: "foo.scala" :: Nil
  val permutations0 = tokens.toSet.subsets.flatMap(_.toList.permutations).toList.distinct

  def runWithCp(cp: String) = {
    val permutations = permutations0.flatMap(s => ("-cp CPTOKEN" :: s).permutations)

    for ((p, i) <- permutations.distinct.sortBy(_ mkString "").zipWithIndex) {
      val args     = p.flatMap(_.split("\\s+")).map(x => if (x == "CPTOKEN") cp else x)
      val expected = args.filter(_ == "foo.scala")

      val s              = new MutableSettings(println)
      val (ok, residual) = s.processArguments(args, processAll = true)

      assert(residual == expected, residual)
      assert(ok, args)
      println(s"$i) $args ==> $s")
    }
  }

  def main(args0: Array[String]): Unit = {
    runWithCp("")
    runWithCp("/tmp:/bippy")
  }
}
