/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;

/** This trait extends the Map interface of collections that unambiguously map
 *  keys to values (i.e. a key is mapped to at least one value).
 *  This trait defines the interface for functional map implementations
 *  relying on immutable data structures.
 *  Concrete map implementations have to provide functionality for the
 *  abstract methods in scala.collection.Map as well as for
 *  <code>factory</code>, <code>update</code>, and -.

 *  @author  Matthias Zenger, Erik Stenman
 *  @version 1.0, 03/12/2003
 */
trait Map[KEY, VALUE] with scala.collection.Map[KEY, VALUE] {

  /** A factory to create empty maps of the same type of keys.
  */
  val factory:MapFactory[KEY];


  /** This method allows one to create a new map with an
  *  additional mapping from <code>key</code>
  *  to <code>value</code>. If the map contains already a
  *  mapping for <code>key</code>, it will be overridden by this
  *  function.
  */
  def update(key: KEY, value: VALUE): Map[KEY, VALUE];

  /** This creates a new mapping without the given <code>key</code>.
  *  If the map does not contain a mapping for the given key, the
  *  method returns the same map.
  */
  def -(key: KEY): Map[KEY, VALUE];

  /** This method defines syntactic sugar for adding a
  *  mapping. It is typically used in the following way:
  *  <pre>
  *  map + key -> value;
  *  </pre>
  */
  def +(key: KEY): MapTo = new MapTo(key);


  /** <code>incl</code> can be used to add many mappings at the same time
  *  to the map. The method assumes that a mapping is represented
  *  by a <code>Pair</code> object who's first component denotes the
  *  key, and who's second component refers to the value.
  */
  def incl(mappings: Pair[KEY, VALUE]*): Map[KEY, VALUE] = incl(mappings);

  /** <code>incl</code> can be used to add many mappings at the same time
  *  to the map. The method assumes that each mapping is represented
  *  by an Iterator over <code>Pair</code> objects who's first component
  *  denotes the key, and who's second component refers to the value.
  */
  def incl(map: Iterable[Pair[KEY, VALUE]]): Map[KEY, VALUE] = {
    val iter = map.elements;
    var res = this;
    while (iter.hasNext) {
      val Pair(key, value) = iter.next;
      res = res.update(key, value);
    }
    res;
  }

  /** This method will return a map where all the mappings
  *  for the given sequence of keys are removed from the map.
  */
  def excl(keys: KEY*): Map[KEY, VALUE] = excl(keys);

  /** This method removes all the mappings for keys provided by an
  *  iterator over the elements of the <code>keys</code> object.
  */
    def excl(keys: Iterable[KEY]): Map[KEY, VALUE] = {
        val iter = keys.elements;
        var res = this;
        while (iter.hasNext) {
            res = res - iter.next;
        }
        res;
    }

     /** This function transforms all the values of mappings contained
     *  in this map with function <code>f</code>.
     */
    def map[C <: Any](f: (KEY, VALUE) => C): Map[KEY, C] = {
        var res = factory.Empty[C];
        elements foreach {
            case Pair(key, value) => res = res.update(key, f(key, value));
        }
        res;
    }

    /** This method removes all the mappings for which the predicate
     *  <code>p</code> returns <code>false</code>.
     */
    def filter(p: (KEY, VALUE) => Boolean): Map[KEY, VALUE] = {
        var res = this;
        toList foreach {
            case Pair(key, value) => if (p(key, value)) { res = res.excl(key); }
        }
        res;
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

  /** Compares two maps for equality.
  *   Two maps are equal iff they contain exactly the
  *   same key-value pairs.
  */
  override def equals(obj: Any): Boolean =
    if (obj.isInstanceOf[scala.collection.Map[KEY, VALUE]]) {
      val that = obj.asInstanceOf[scala.collection.Map[KEY, VALUE]];
      if (size != that.size) false else elements.forall {
        case Pair(key, value) => that.get(key) match {
          case None => false;
          case Some(v) => v == value;
        }
      };
    } else
      false;

  /** This method controls how a mapping is represented in the string
  *  representation provided by method <code>toString</code>.
  */
  def mappingToString(p: Pair[KEY, VALUE]) = p._1.toString() + " -> " + p._2;

    class MapTo(key: KEY) {
        def ->(value: VALUE): Map[KEY, VALUE] = update(key, value);
    }
}

abstract class MapFactory[KEY] {
  def Empty[VALUE]:Map[KEY,VALUE];
}
