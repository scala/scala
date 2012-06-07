import language.implicitConversions

class annot(a: String) extends annotation.StaticAnnotation

class C[@annot(false) X] {
  implicit def b2s(b: Boolean): String = ""
}
