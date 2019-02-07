// scalac: --deprecation-policy:+policy.test,-policy.reprieve,'oldtrial
//
// also can match on since, but note annoying spaces
// --deprecation-policy:+policy.test since=Policy 1.0,-policy.reprieve since<1.5,'oldtrial

package policy.test {
  object X {
    @deprecated("Badly done", since="Policy 1.0")
    def x = 42
  }
}
package policy.reprieve {
  object Y {
    @deprecated("Also pretty bad but salvageable", since="Policy 1.0")
    def y = 42
  }
}
package policy.stale {
  object Z {
    @deprecated("Let it die", since="Policy 1.0")
    def z = 42
  }
}

package trial {
  trait T {
    def f = policy.test.X.x
    def g = policy.reprieve.Y.y
  }
}
package oldtrial {
  trait T {
    // TODO
    def old = policy.stale.Z.z
  }
}
