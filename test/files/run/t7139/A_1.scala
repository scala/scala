package test {
  object A {
    def apply(n: A) = n
  }
}
package object test {
  type A = Int
}
