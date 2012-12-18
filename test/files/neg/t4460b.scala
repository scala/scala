trait A

class Outer() {
	class B(val x: Int) {
	  self: A =>

	  def this() = this() // was binding to Predef.<init> !!
	}
}
