//> using options -Xsource:3
case class C(i: Int)(j: => Int)(k: => Int) { def sum = i + j + k }
