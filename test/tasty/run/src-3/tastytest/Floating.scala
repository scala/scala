package tastytest

trait Floating {
  @annotation.strictfp def add(a: Float, b: Float) = a + b
}
