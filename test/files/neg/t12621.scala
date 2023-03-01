
// scalac: -Xsource:3

class C {
  def f(sb: StringBuilder): sb.type = sb
}

class C2 extends C {
  override def f(xb: StringBuilder) = new StringBuilder
}
