
//> using options -Werror

package testsamepackageimport {
  package p {
    class C
  }

  package p {
    package q {
      import p._ // no warn
      class U {
        def f = new C
      }
    }
  }
}
