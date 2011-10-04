package com.example {
  package object p1 {
    def packageObjectMethod = 0
  }
}

package com.example.p1 {
  class Clazz {
    def foo = packageObjectMethod
    implicit def intToClass1(n: Int) = new Clazz
  }

  class UpperBound[T <: Int]

  class LowerBound[T >: Int]

  class ExistentialType {
    def foo(array: Array[T] forSome { type T <: AnyVal }) = 0
  }
}
