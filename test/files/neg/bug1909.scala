// Until #1909 is fixed, if this compiles the bytecode
// will trigger a VerifyError.
class Class {
  def this(value: Int) = this()
  def this(p: String) = this(try 0)
}