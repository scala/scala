/*
rule = "scala:fix.Collectionstrawman_v0"
 */
package fix

object Collectionstrawman_v0_Stream {
  Stream(1, 2, 3)
  1 #:: 2 #:: 3 #:: Stream.Empty
  val isEmpty: Stream[_] => Boolean = {
    case Stream.Empty => true
    case x #:: xs     => false
  }
}
