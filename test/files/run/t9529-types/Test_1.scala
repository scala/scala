/* evidently annotations on types don't make it into bytecode yet, even though
 * such a thing is allowed in Java 8 and onwards. Here's a test that it'll work
 * with repeatable annotations anyways.
 *
 * nb. currently multiple annotations on type trees get reversed by typer
 */

import scala.tools.partest._

import anns._

@TypeAnn_0("")
object Test extends DirectTest {

  override def extraSettings: String =
    s"-usejavacp -cp ${testOutput.path} -Xprint:pic,ref -Ystop-after:ref -d ${testOutput.path}"

  override def code =
    """import anns._
      |trait Foo extends (
      |  (Int @TypeAnn_0("a") @TypeAnn_0("b"))
      |    => (String @TypeAnn_0("x") @TypeAnn_0("y"))
      |) {
      |  type Meh = Any@TypeAnn_0("p")@TypeAnn_0("q")
      |}
    """.stripMargin

  override def show() = compile()
}
