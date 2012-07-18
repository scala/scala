import language.implicitConversions

class annot(a: String) extends annotation.StaticAnnotation

class C[@annot(false) X] {
  implicit def b2s(b: Boolean): String = ""
}

class D[@annot(b2s(false)) X] {
  implicit def b2s(b: Boolean): String = ""
}

@annot(false) class E {
  implicit def b2s(b: Boolean): String = ""
}

@annot(b2s(false)) class F {
  implicit def b2s(b: Boolean): String = ""
}

object T {
  implicit def b2s(b: Boolean): String = ""
  @annot(false) val x = 0
  @annot(b2s(false)) val y = 0
}
