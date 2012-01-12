package object foo {
  implicit def x123: Bippy = new Bippy("x")
}
package foo {
  class Bippy(override val toString: String){ }
  class Dingus {
    def f1 = {
      implicit def z: Bippy = new Bippy("z")
      implicitly[Bippy]
    }
  }
  object Test extends App {
    println(new Dingus().f1)
  }
}
