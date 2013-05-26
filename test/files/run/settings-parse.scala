
import scala.language.postfixOps
import scala.tools.nsc._

object Test {
  val tokens        = List("", "-deprecation", "foo.scala")
  val subsets       = tokens.toSet.subsets.toList
  val permutations0 = subsets.flatMap(_.toList.permutations).distinct

  def runWithCp(cp: String) = {
    val permutations = permutations0 flatMap ("-cp CPTOKEN" :: _ permutations)

    for ((p, i) <- permutations.distinct.sortBy(_ mkString "").zipWithIndex) {
      val args           = p flatMap (_ split "\\s+") map (x => if (x == "CPTOKEN") cp else x)
      val s              = new settings.MutableSettings(println)
      val (ok, residual) = s.processArguments(args, processAll = true)

      val expected = args filter (_ == "foo.scala")
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
