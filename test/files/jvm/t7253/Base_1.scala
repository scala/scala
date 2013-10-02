trait A { def f(): Int }
trait B1 extends A
abstract class B2 extends A
class B3 extends A { def f(): Int = 1 }
class B4 extends B3
