class B(x : () => Int)
class A(i : Int) extends B(() => i) { i }
