trait ValueDiscard[@specialized U] {
  def u: U
}
/* Was:
scalac-hash v2.11.5 -Ywarn-value-discard test/files/pos/t9020.scala
test/files/pos/t9020.scala:2: warning: discarded non-Unit value
  def u: U
      ^
one warning found
*/
