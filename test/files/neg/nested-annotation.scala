import annotation._

class ComplexAnnotation(val value: Annotation) extends ClassfileAnnotation

class A {
  // It's hard to induce this error because @ComplexAnnotation(@inline) is a parse
  // error so it never gets out of the parser, but:
  @ComplexAnnotation(new inline) def bippy(): Int = 1
}
