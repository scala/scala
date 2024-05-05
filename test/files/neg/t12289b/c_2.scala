//> using options -d /tmp -Yskip:jvm
// -d is required to permit helpful output.
// compilation will fail but ensure no output files for this round.
package p {
  trait T
  class C extends T {
    def companion: AnyRef = T
  }
}
