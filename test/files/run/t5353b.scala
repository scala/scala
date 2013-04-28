import scala.tools.partest._

object Test extends CompilerTest {
  import global._

  override def code = "class A" // dummy

  def tp[T: TypeTag] = typeOf[Array[T]]

  def check(source: String, unit: global.CompilationUnit) {
    // Bug: Array[String] explodes due to type alias Predef.String
    val tps = List(
      tp[Array[Int]], tp[Array[java.lang.String]],
      tp[Nothing], tp[Null], tp[Int], tp[Long], tp[java.lang.String],
      tp[java.lang.Object], tp[Any]
    ) sortBy (_.toString)

    val pairs = for (t1 <- tps; t2 <- tps; if t1.## <= t2.##) yield ((t1, t2))

    for ((t1, t2) <- pairs) {
      val lub1 = exitingTyper(lub(List(t1, t2)))
      val lub2 = exitingPostErasure(lub(List(t1, t2)))
      val str1 = f"$t1%20s and $t2%-20s"
      val str2 = f"$lub1%20s <and> $lub2%-20s"
      println(f"pre/post erasure lubs of  $str1  are  $str2".trim)
    }
  }
}
