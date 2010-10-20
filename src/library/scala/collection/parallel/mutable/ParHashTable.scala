package scala.collection
package parallel.mutable




import collection.mutable.HashEntry




/** Provides functionality for hash tables with linked list buckets,
 *  enriching the data structure by fulfilling certain requirements
 *  for their parallel construction and iteration.
 */
trait ParHashTable[K] {

  protected type Entry >: Null <: HashEntry[K, Entry]

}





