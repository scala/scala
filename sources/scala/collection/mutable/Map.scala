/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable;


/** This trait represents mutable maps. Concrete map implementations
 *  just have to provide functionality for the abstract methods in
 *  <code>scala.collection.Map</code> as well as for <code>update</code>,
 *  and <code>remove</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait Map[A, B] with scala.collection.Map[A, B] {

    /** This method allows one to add a new mapping from <code>key</code>
     *  to <code>value</code> to the map. If the map contains already a
     *  mapping for <code>key</code>, it will be overridden by this
     *  function.
     */
    def update(key: A, value: B): Unit;

    /** This method removes a mapping from the given <code>key</code>.
     *  If the map does not contain a mapping for the given key, the
     *  method does nothing.
     */
    def -=(key: A): Unit;

    /** This method defines syntactic sugar for adding or modifying
     *  mappings. It is typically used in the following way:
     *  <pre>
     *  map += key -> value;
     *  </pre>
     */
    def +=(key: A): MapTo = new MapTo(key);

    /** <code>incl</code> can be used to add many mappings at the same time
     *  to the map. The method assumes that a mapping is represented
     *  by a <code>Pair</code> object who's first component denotes the
     *  key, and who's second component refers to the value.
     */
    def incl(mappings: Pair[A, B]*): Unit = {
        val ys = mappings.asInstanceOf[List[Pair[A, B]]];
        ys foreach { case Pair(key, value) => update(key, value); };
    }

    /** This method adds all the mappings provided by an iterator of
     *  parameter <code>map</code> to the map.
     */
    def incl(map: Iterable[Pair[A, B]]): Unit = map.elements foreach {
        case Pair(key, value) => update(key, value);
    }

    /** This method will remove all the mappings for the given sequence
     *  of keys from the map.
     */
    def excl(keys: A*): Unit = excl(keys);

    /** This method removes all the mappings for keys provided by an
     *  iterator over the elements of the <code>keys</code> object.
     */
    def excl(keys: Iterable[A]): Unit = keys.elements foreach -=;

    /** Removes all mappings from the map. After this operation is
     *  completed, the map is empty.
     */
    def clear: Unit = keys foreach -=;

    /** This function transforms all the values of mappings contained
     *  in this map with function <code>f</code>.
     */
    def map(f: (A, B) => B): Unit = elements foreach {
        case Pair(key, value) => update(key, f(key, value));
    }

    /** This method removes all the mappings for which the predicate
     *  <code>p</code> returns <code>false</code>.
     */
    def filter(p: (A, B) => Boolean): Unit = toList foreach {
        case Pair(key, value) => if (p(key, value)) -=(key);
    }

    /** Returns a string representation of this map which shows
     *  all the mappings.
     */
    override def toString() =
        if (size == 0)
            "{}"
        else
            "{" + {
                val iter = elements;
                var res = mappingToString(iter.next);
                while (iter.hasNext) {
                    res = res + ", " + mappingToString(iter.next);
                }
                res;
            } + "}";

    /** This method controls how a mapping is represented in the string
     *  representation provided by method <code>toString</code>.
     */
    def mappingToString(p: Pair[A, B]) = p._1.toString() + " -> " + p._2;

    private class MapTo(key: A) {
        def ->(value: B): Unit = update(key, value);
    }
}
