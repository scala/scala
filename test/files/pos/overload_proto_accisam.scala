// TODO make independent of java.io.OutputStream, but obvious way does not capture the bug (see didInferSamType and OverloadedArgProto)
class Test {
  def overloadedAccidentalSam(a: java.io.OutputStream, b: String) = ???
  def overloadedAccidentalSam(a: java.io.OutputStream, b: Any)= ???

  overloadedAccidentalSam(??? : java.io.OutputStream, null)
}
