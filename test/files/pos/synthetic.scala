// scalac: -deprecation -Xsource:2.14 -Werror
//
// Ensure synthetic hashCode compiles cleanly.

case class C(c: Int) extends AnyVal
