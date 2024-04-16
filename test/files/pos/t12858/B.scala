//> using options -Werror

trait B1 extends A { def f: Int }
trait C1 { def f = 2 }
class T1 extends B1 with C1

trait B2 extends A { def f: Int = 1}
trait C2 { self: B2 => override def f = 2 }
class T2 extends B2 with C2
