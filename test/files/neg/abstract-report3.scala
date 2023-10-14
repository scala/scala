
trait U { def t(): Int }
trait T extends U { abstract override def t(): Int = super.t() + 1 }
abstract class C extends T      // mixin
