package scala
package reflect.io

import scala.reflect.internal.util.Statistics

// Due to limitations in the Statistics machinery, these are only
// reported if this patch is applied.
//
// --- a/src/reflect/scala/reflect/internal/util/Statistics.scala
// +++ b/src/reflect/scala/reflect/internal/util/Statistics.scala
// @@ -109,7 +109,7 @@ quant)
//     *  Quantities with non-empty prefix are printed in the statistics info.
//     */
//    trait Quantity {
// -    if (enabled && prefix.nonEmpty) {
// +    if (prefix.nonEmpty) {
//        val key = s"${if (underlying != this) underlying.prefix else ""}/$prefix"
//        qs(key) = this
//      }
// @@ -243,7 +243,7 @@ quant)
//     *
//     *  to remove all Statistics code from build
//     */
// -  final val canEnable = _enabled
// +  final val canEnable = true // _enabled
//
// We can commit this change as the first diff reverts a fix for an IDE memory leak.
private[io] object IOStats {
  val fileExistsCount      = Statistics.newCounter("# File.exists calls")
  val fileIsDirectoryCount = Statistics.newCounter("# File.isDirectory calls")
  val fileIsFileCount      = Statistics.newCounter("# File.isFile calls")
}
