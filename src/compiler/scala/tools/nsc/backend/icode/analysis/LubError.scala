package scala.tools.nsc.backend.icode.analysis;

class LubError(a: Any, b: Any, msg: String) extends Exception {
  override def toString() = "Lub error: " + msg + a + b;
}
