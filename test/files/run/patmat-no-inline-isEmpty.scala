import scala.tools.partest._

object Test extends DirectTest {
  def depCode =
    """class Wrap(private val a: Int) extends AnyVal {
      |  def isEmpty: false               = { println("confirm seq isEmpty method doesn't get elided"); false }
      |  def get                          = this
      |  def lengthCompare(len: Int)      = Integer.compare(1, len)
      |  def apply(i: Int)                = if (i == 0) a else Nil(i)
      |  def drop(n: Int): scala.Seq[Int] = if (n == 0) toSeq else Nil
      |  def toSeq: scala.Seq[Int]        = List(a)
      |}
    """.stripMargin

  override def code =
    """object A {
      |  def unapplySeq(a: Int) = new Wrap(a)
      |}
      |class T {
      |  def t: Any = 2 match {
      |    case A(xs @ _*) => xs
      |    case _          => "other"
      |  }
      |}
    """.stripMargin

  def show(): Unit = Console.withErr(System.out) {
    compileString(newCompiler("-usejavacp"))(depCode)
    compileString(newCompiler("-usejavacp", "-cp", testOutput.path, "-Vprint:patmat"))(code)
  }
}
