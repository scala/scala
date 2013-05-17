trait A

class B(val x: Int) {
  self: A =>

  def this() = this() // was binding to Predef.<init> !!
}
