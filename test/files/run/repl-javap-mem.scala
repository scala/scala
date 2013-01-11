import scala.tools.partest.JavapTest

object Test extends JavapTest {
  def code = """
    |object Betty {
    |  val ds = List(1,2,3) filter (_ % 2 == 0) map (_ * 3)
    |  def m(vs: List[Int]) = vs filter (_ % 2 != 0) map (_ * 2)
    |}
    |:javap Betty#m
  """.stripMargin

  // filter for requested method member
  override def yah(res: Seq[String]) = {
    // cheaply, methods end in arg list
    val p = """.*m\(.*\);""".r
    def filtered = res filter (_ match { case p() => true case _ => false })
    1 == filtered.size
  }
}
