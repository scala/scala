package scala.test.scaladoc.prefix {
  package pack1 {

    class A {
      class Z
    }

    class B extends A

    package a {
      class C
    }

    package b {
      class C
    }

    package c {
      class C

      class L extends pack2.Z

      class TEST {
        // test inherited classes
        def fooCA(x: pack1.A#Z) = 1
        def fooCB(x: pack1.B#Z) = 1
        def fooCS(x: pack2.Z#Z) = 1
        def fooCL(x: L#Z) = 1
        // test in packages
        def fooPA(x: pack1.a.C) = 1
        def fooPB(x: pack1.b.C) = 1
        def fooPC(x: pack1.c.C) = 1
      }

      class A extends pack1.A
    }
  }

  package pack2 {
    class Z extends pack1.A
  }
}