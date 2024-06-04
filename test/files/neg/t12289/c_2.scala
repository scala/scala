//> using options -d /tmp -Yskip:jvm
// -d is required to permit helpful output.
// compilation will fail but ensure no output files for this round.
package p {
  object T
  class C extends T
}
