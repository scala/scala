import scala.tools.partest._

object Test extends StoreReporterDirectTest {
  override def extraSettings: String = "-usejavacp -Xprint:typer -Ystop-after:typer"

  override def code =
    """@scala.annotation.meta.field class blort extends scala.annotation.StaticAnnotation
      |class C1 {
      |  @blort val foo = "hi"
      |}
      |object X {
      |  def accessIt(c: C2) = c.foo
      |}
      |class C2 extends C1 {
      |  @blort override val foo = "bye"
      |}
    """.stripMargin

  def show(): Unit = {
    val baos = new java.io.ByteArrayOutputStream()
    Console.withOut(baos)(Console.withErr(baos)(compile()))
    val out = baos.toString("UTF-8")

    val fooDefs = out.lines.filter(_.contains("private[this] val foo")).map(_.trim).toList
    assert(fooDefs.length == 2)
    assert(fooDefs.forall(_.startsWith("@blort private[this] val foo: String =")), fooDefs)
  }
}
