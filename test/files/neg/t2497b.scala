// from https://github.com/scala/scala/pull/7439#issuecomment-470101184
trait A { def f: Int }
trait AA extends A { def f = -1 }
trait AAA extends A { override def f = 1 }
trait B { def f = -1 }
class C extends B with AAA
