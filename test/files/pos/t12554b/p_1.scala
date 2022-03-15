
package p {
  object X
}

package object q {
  object Y
}

package q {
  class C
}

package object r {
  object Z {
    def greeting = "hello, world"
  }
}
