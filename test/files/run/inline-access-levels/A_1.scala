package test

object A {
  private[test] var packagePrivate: Int = -1
}

trait A1 {
  private[test] var packagePrivateClass: Int = -1
}

class A extends A1 {
//  private[test] var packagePrivateClass: Int = -1
}
