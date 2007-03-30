package scala;

/** Variant of <code>Iterable</code> used to describe
 *  collections with a finite number of elements.
 *  Basically, this trait just adds size and toString to Iterable,
 *  as most of the methods in Iterable already assume finite-ness.
 *
 *  @author Sean McDirmid
 */
trait Collection[+A] extends Iterable[A] {
  /** Returns the number of elements in this collection.
    *
    *  @return number of collection elements.
    */
  def size : Int
  /** Converts this iterable to a fresh Array with elements.
    *
    * @deprecated This method is broken for BitSet. BitSet.toArray will be updated to new behavior in a future release.
    */
  @deprecated def toArray[B >: A]: Array[B] = toList.toArray

  override def toString = mkString(stringPrefix + "(", ", ", ")")

  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  protected def stringPrefix : String = {
    val string = getClass.getName
    val idx = string.lastIndexOf('.' : Int)
    if (idx != -1) string.substring(idx + 1)
    else string
  }

}