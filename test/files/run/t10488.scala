

import javax.script._

object Test {
  def run() = {
    val sem = new ScriptEngineManager()
    val eng = sem.getEngineByName("scala")
    assert(eng != null)
    assert(eng.eval("42", eng.getContext).asInstanceOf[Int] == 42)
  }
  def main(args: Array[String]): Unit = run()
}
