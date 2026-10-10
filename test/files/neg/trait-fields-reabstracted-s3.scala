//> using options -Xsource:3
// Scala 3 rejects a def re-declaring a concrete val, and a re-declared final member;
// -Xsource:3 reports both (migration warnings are errors under -Xsource:3).
trait T { val m: String = "T"; final def f: Int = 1 }

abstract class B extends T { def m: String }
abstract class B2 extends T { def f: Int }
abstract class B3 extends T { val m: String } // fine: an abstract val may re-declare a val
