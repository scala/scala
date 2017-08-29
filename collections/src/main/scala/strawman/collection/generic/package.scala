package strawman.collection

import scala.deprecated

package object generic {
  @deprecated("Clearable was moved from collection.generic to collection.mutable", "2.13.0")
  type Clearable = strawman.collection.mutable.Clearable
}
