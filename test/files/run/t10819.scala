import scala.tools.partest._

object Test extends StoreReporterDirectTest {
  override def extraSettings: String = "-usejavacp -Xprint:typer -Ystop-after:typer"

  override def code =
    """class C {
      |  def id[T <: AnyVal](x: T): T = x
      |  id(if (1 == 1) 1 else 2) // should infer id[Int], not id[AnyVal]
      |}
      |
    """.stripMargin

  def show(): Unit = {
    val baos = new java.io.ByteArrayOutputStream()
    Console.withOut(baos)(Console.withErr(baos)(compile()))
    val out = baos.toString("UTF-8")

    val inferredType = out.linesIterator.filter(_.contains("C.this.id[")).map(_.trim).toList
    assert(inferredType.length == 1)
    assert(inferredType.forall(_.startsWith("C.this.id[Int]")), inferredType)
  }
}
