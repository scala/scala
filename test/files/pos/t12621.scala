
// scalac: -Xsource:3

class C {
  def f(sb: StringBuilder): sb.type = sb
}

class C1 extends C {
  override def f(yb: StringBuilder): yb.type = yb   // explicit already ok
}

class C2 extends C {
  override def f(xb: StringBuilder) = xb    // inferred StringBuilder
}

class D {
  val x: StringBuilder = new StringBuilder
  def f(sb: x.type): sb.type = sb
}

class D1 extends D {
  override def f(sb: x.type) = sb   // stable type already ok
}

class Wrapper {
  val sb: StringBuilder = new StringBuilder
}

class E {
  def f(w: Wrapper): w.sb.type = w.sb
}

class E1 extends E {
  override def f(w: Wrapper) = w.sb   // already ok
}

/*
was:
t12621.scala:13: error: incompatible type in overriding
def f(sb: StringBuilder): sb.type (defined in class C);
 found   : (sb: StringBuilder): StringBuilder
 required: (sb: StringBuilder): sb.type
  override def f(sb: StringBuilder) = sb
               ^
 */
