package demo.sub

class A {
	implicit def x(i: Int): C = new C(i)
}
class C(i: Int) {
	def y = i + 2
}
