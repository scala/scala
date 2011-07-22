/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

/** A trait for all maps upon which operations may be
 *  implemented in parallel.
 *
 *  @define Coll GenMap
 *  @define coll general map
 *  @author Martin Odersky
 *  @author Aleksandar Prokopec
 *  @since 2.9
 *  @define mapNote
 *
 *  A map is a collection of bindings from keys to values, where there are
 *  no duplicate keys.
 */
trait GenMapLike[A, +B, +Repr] extends GenIterableLike[(A, B), Repr] with Equals with Parallelizable[(A, B), parallel.ParMap[A, B]] {
  def default(key: A): B
  def get(key: A): Option[B]
  def apply(key: A): B
  def seq: Map[A, B]
  def +[B1 >: B](kv: (A, B1)): GenMap[A, B1]
  def - (key: A): Repr

  // This hash code must be symmetric in the contents but ought not
  // collide trivially.
  override def hashCode() = util.MurmurHash.symmetricHash(seq, Map.hashSeed)

  /** Compares two maps structurally; i.e. checks if all mappings
   *  contained in this map are also contained in the other map,
   *  and vice versa.
   *
   *  @param that the other map
   *  @return     `true` if both maps contain exactly the
   *              same mappings, `false` otherwise.
   */
  override def equals(that: Any): Boolean = that match {
    case that: GenMap[b, _] =>
      (this eq that) ||
      (that canEqual this) &&
      (this.size == that.size) && {
      try {
        this forall {
          case (k, v) => that.get(k.asInstanceOf[b]) match {
            case Some(`v`) =>
              true
            case _ => false
          }
        }
      } catch {
        case ex: ClassCastException =>
          println("class cast "); false
      }}
    case _ =>
      false
  }
}
