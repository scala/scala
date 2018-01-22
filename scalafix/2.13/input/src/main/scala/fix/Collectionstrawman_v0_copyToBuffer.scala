/*
rule = "scala:fix.Collectionstrawman_v0"
 */
package fix

import scala.collection.mutable

class Collectionstrawman_v0_copyToBuffer(xs: List[Int], b: mutable.Buffer[Int]) {

  xs.copyToBuffer(b)
  (xs ++ xs).copyToBuffer(b)

}
