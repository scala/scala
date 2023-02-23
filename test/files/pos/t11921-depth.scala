// scalac: -Xfatal-warnings -Xsource:2.13

package p.q.test {
  class K
}

package test {
  class NimicDeloc
}

package m {
  import p.q._

  abstract class T {
    def test = 1
  }

  class C extends T {
    def m = test
  }
}
