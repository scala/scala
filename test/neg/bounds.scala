class I {
	def i(): Unit = {}
}
class J extends I {
     def j(): Unit = {}
}
class A[T >: I](init: T) {
     var y: T = new B();
	class B() extends I {}
}
class B[T >: J](init: T) extends A[T](init) {
}
