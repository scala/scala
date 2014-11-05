private object A {
  private class B
}

// was: class B in object A cannot be accessed in object A
class A private(b: A.B)
