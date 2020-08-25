// scalac: -Xsource:2.11
trait F { def apply(x: Int): String }

class C { val f: F = x => "a" }
