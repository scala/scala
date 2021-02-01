import scala.tools.partest._
object Test extends CompilerTest {
  import global._
  override def extraSettings = super.extraSettings + " -Yrangepos"
  override def sources = List(
    number("TestByte", "val value:Byte  = 1.toByte"),
    number("TestShort", "val value:Short  = 1.toShort"),
    number("TestInt", "val value:Int  = 1.toInt"),
    number("TestLong", "val value:Long  = 1.toLong"),
    primatives("TestBoolean", "val value: Boolean = true"),
    primatives("TestChar", "val value:Char  = 'x'"),
    floating("TestFloat", "val value:Float  = 1.toFloat"),
    floating("TestDouble", "val value:Double  = 1.toDouble")
    )
  def primatives(className: String, decl: String) = {
    s"""|
        |class $className {
        |  $decl
        |
        |
        |  val a1 = value.toString
        |  val a2 = value.hashCode
        |  val a3 = value.##
        |}""".stripMargin

  }
  def number(className: String, decl: String) = {
    s"""|
        |class $className {
        |  $decl
        |
        |
        |  val a1 = value.toString
        |  val a2 = value.hashCode
        |  val a3 = value.##
        |
        |  val c1 = value.floatValue
        |  val c2 = value.doubleValue
        |  val c3 = value.longValue
        |  val c4 = value.intValue
        |  val c5 = value.shortValue
        |  val c6 = value.byteValue
        |
        |  val d1 = value max value
        |  val d2 = value min value
        |  val d3 = value.abs
        |  val d4 = value.signum
        |
        |  val e1 = value.toByte
        |  val e2 = value.toShort
        |  val e3 = value.toInt
        |  val e4 = value.toLong
        |  val e5 = value.toFloat
        |  val e6 = value.toDouble
        |
        |}""".stripMargin

  }
  def floating(className: String, decl: String) = {
    1.abs
    s"""|
       |class $className {
        |  $decl
        |
        |
        |  val a1 = value.toString
        |  val a2 = value.hashCode
        |  val a3 = value.##
        |
        |  val b1 = value.isNaN
        |  val b2 = value.isInfinity
        |  val b3 = value.isInfinite
        |  val b4 = value.isNegInfinity
        |  val b5 = value.isPosInfinity
        |
        |  val c1 = value.floatValue
        |  val c2 = value.doubleValue
        |  val c3 = value.longValue
        |  val c4 = value.intValue
        |  val c5 = value.shortValue
        |  val c6 = value.byteValue
        |
        |  val d1 = value max value
        |  val d2 = value min value
        |  val d3 = value.abs
        |  val d4 = value.signum
        |  val d5 = value.round
        |  val d6 = value.ceil
        |  val d7 = value.floor
        |  val d8 = value.toRadians
        |  val d9 = value.toDegrees
        |
        |  val e1 = value.toByte
        |  val e2 = value.toShort
        |  val e3 = value.toInt
        |  val e4 = value.toLong
        |  val e5 = value.toFloat
        |  val e6 = value.toDouble
        |
        |}""".stripMargin

  }
  def check(source: String, unit: CompilationUnit) {
    //really we are checking for calls tht box things
    //e.g.
    // scala.Int.box
    // doubletoDouble etc
    // easest way to see this is to print all the efs, and that should show any references
    println()
    for (ClassDef(_, className, _, Template(_, _, stats)) <- unit.body) {
      println(s"class $className")
      for (stat <- stats;
           t <- stat) {
        t match {
          case _: Apply =>
            val pos = t.pos
            println(s"source-${pos.source.path},line-${pos.line} $t")
          case _                              =>
        }
      }
    }
    println()
  }
}
