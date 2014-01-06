import scala.tools.partest._

// Type constructors for FunctionN and TupleN should not be considered as function type / tuple types.
object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp"

  def code = ""

  def show() {
    val global = newCompiler()
    new global.Run()
    import global._, definitions._
    val function0TC = FunctionClass(0).typeConstructor
    val tuple1TC = TupleClass(1).typeConstructor
    FunctionClass.seq.foreach { sym =>
      val tc = sym.typeConstructor
      assert(!isFunctionType(tc), s"$tc")
      assert(!isFunctionTypeDirect(tc), s"$tc (direct)")
    }
    TupleClass.seq.foreach { sym =>
      val tc = sym.typeConstructor
      assert(!isTupleType(tc), s"$sym")
      assert(!isTupleTypeDirect(tc), s"$tc (direct)")
    }
  }
}
