import scala.language.dynamics

class DynTest extends Dynamic {
  def applyDynamicNamed(name: String)(values: Seq[(String, Any)]) = "test"
}

class CompilerError {
  val test = new DynTest
  test.crushTheCompiler(a = 1, b = 2)
}