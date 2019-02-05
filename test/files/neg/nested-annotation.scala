import annotation._

class ComplexAnnotation(val value: Any) extends ConstantAnnotation

class A {
  // It's hard to induce this error because @ComplexAnnotation(@inline) is a parse
  // error so it never gets out of the parser, but:
  @ComplexAnnotation(new inline) def bippy(): Int = 1

  // No error here, as `SuppressWarnings` is defined in Java
  @ComplexAnnotation(new SuppressWarnings(Array("blup"))) def huppy(): Int = 2
}
