import scala.tools.partest.DirectTest

object Test extends DirectTest {

  override def extraSettings: String =
    s"-usejavacp -Vprint-pos -Vprint:parser -Yrangepos -Ystop-after:parser -cp ${testOutput.path}"

  // test/files/pos/t6124.scala
  override def code = """
    trait T {
      def i: Int = 1_024
      def j: Long = 1_024L * 1_024
      //def k = 1'024

      def f = 3_14e-2
      def d = 3_14E-2_1

      def z = 0
    }
  """.trim

  override def show(): Unit = Console.withErr(System.out) {
    compile()
  }
}
