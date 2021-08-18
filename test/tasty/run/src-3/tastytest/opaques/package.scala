package tastytest

package object opaques {
  opaque type Offset = Long
  object Offset:
    def apply(o: Long): Offset = o
}
