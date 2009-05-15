package test

/* Tests correct generation of Java signatures. The local method 'bar' should
 * not get a generic signature, as it may refer to type parameters of the enclosing
 * method, and the JVM does not know about nested methods.
 */
class Outer {
  def foo[A, B](x: A, y: B) = {
    def bar[X](x1: A, y1: B, z1: X) = z1
    bar(x, y, 10)
  }
}
