package fix

import scala.collection.mutable

class Collectionstrawman_v0_copyToBuffer(xs: List[Int], b: mutable.Buffer[Int]) {

  b ++= xs
  b ++= xs ++ xs

}
