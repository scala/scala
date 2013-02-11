class A[m[+x]] {
 def str: m[Object] = sys.error("foo")
}

class B[m[+x]] extends A[m] {
 override def str: m[String]  = sys.error("foo")
}
