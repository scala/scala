abstract class ParametricMessage[M: Manifest](msg: M) { def message = msg }
case class ParametricMessage1[M: Manifest](msg: M, p1: Class[_]) extends ParametricMessage(msg)
