class A(x: String, y: Int)(implicit o: String)
class B1(implicit o: String) extends A(y = 5, x = "a")
class B2(implicit o: String) extends A("a", 5)
class B3(implicit o: String) extends A(y = 5, x = "a")(o)

class AM[E: Manifest](val x: Unit = (), y: Unit)
class BM[E: Manifest] extends AM[E](y = ())
