/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala;

/** I promise, there will be some documentation soon! :-) Matthias
 */
class HashMap[A, B] extends MutableMap[A, B] {

    /** The load factor for the hash table.
     */
    protected val loadFactor: Float = 0.75f;

    /** The initial size of the hash table.
     */
   	protected val initialSize: Int = 16;

    /** The actual hash table.
     */
  	protected var table: Array[List[Entry]] = new Array(initialSize);
  	initTable(initialSize - 1);

    /** The number of mappings contained in this hash table.
     */
    protected var tableSize: Int = 0;

    /** The next size value at which to resize (capacity * load factor).
     */
    protected var threshold: Int = ((initialSize as Float) * loadFactor) as Int;

    /** Returns the size of this hash map.
     */
    def size = tableSize;

    /** Returns <tt>true</tt> if this map contains no mappings.
     */
    def isEmpty = (tableSize == 0);

    def apply(key: A) = table(index(key)).find(entryFor(key)) match {
      case None => null
      case Some(e) => e.value
    }

    def get(key: A) = table(index(key)).find(entryFor(key)) match {
      case None => None
      case Some(e) => Some(e.value);
    }

    def update(key: A, value: B) = {
        val h = index(key);
        System.out.println("update(" + key + "<" + h + ">, " + value + ")");
        table(h).find(entryFor(key)) match {
            case None => addEntry(key, value, h);
            case Some(e) => e.value = value;
       	}
    }

    def put(key: A, value: B) = {
    	val old = apply(key);
    	update(key, value);
    	old;
    }

    /*
    def +=(key: A) = new AssignValue(key);

    def +=(map: Map[A, B]) = {
    	val iter = map.iterator;
    	while (iter.hasNext) {
    		val Pair(key, value) = iter.next;
    		this(key) = value;
    	}
    }
    */

    def remove(key: A) = {
    	val old = get(key);
    	old match {
    		case None => null
    		case Some(value) => {
    			val idx = index(key);
    			table(idx) = table(idx).filter(e => e.key != key);
    			tableSize = tableSize - 1;
    			value;
    		}
    	}
    }

    def isDefinedAt(key: A) = table(index(key)).exists(entryFor(key));

    def contains(key: A) = isDefinedAt(key);

    def clear = {
    	initTable(table.length - 1);
    	tableSize = 0;
    }

    def keys = new Iterator[A] {
    	val iter = entries;
  		def hasNext = iter.hasNext;
    	def next = iter.next.key;
  	}

  	def values = new Iterator[B] {
  		val iter = entries;
  		def hasNext = iter.hasNext;
    	def next = iter.next.value;
  	}

  	def iterator = new Iterator[Pair[A, B]] {
  		val iter = entries;
  		def hasNext = iter.hasNext;
    	def next = iter.next.toPair;
  	}

  	override def toString() =
  		if (tableSize == 0) "{}"
  		else "{" + {
  			val iter = entries;
  			var res = iter.next.toString();
  			while (iter.hasNext) {
  				res = res + ", " + iter.next;
  			}
  			res;
  		} + "}";

  	class AssignValue(key: A) {
  		def ->(value: B) = update(key, value);
 	}

    protected class Entry(k: A, v: B) {
    	def key = k;
    	var value = v;
    	def toPair = Pair(k, value);
    	override def toString() = k.toString() + " -> " + value;
    }

  	protected def entries = new Iterator[Entry] {
  		val iterTable = table;
  		var idx = table.length - 1;
  		var xs = iterTable(idx);
  		scan();
  		def hasNext = !xs.isEmpty;
  		def next = {
  			val res = xs.head;
  			xs = xs.tail;
  			scan();
  			res;
  		}
  		def scan(): Unit = if (xs.isEmpty && (idx > 0)) {
  			idx = idx - 1;
  			xs = iterTable(idx);
  			scan();
  		}
  	}

    private def entryFor(key: A) = (e: Entry => e.key == key);

    private def initTable(n: Int): Unit = {
        table(n) = Nil;
        if (n > 0) initTable(n - 1);
    }

    private def addEntry(key: A, value: B, hash: Int) = {
    	table(hash) = (new Entry(key, value)) :: table(hash);
    	tableSize = tableSize + 1;
    	if (tableSize > threshold)
    	    resize(2 * table.length);
    }

    private def resize(newSize: Int) = {
        val newTable: Array[List[Entry]] = new Array(newSize);
        initTable(newSize - 1);
        def rehash(i: Int) = {
        	if (i >= 0)
        		rehashList(table(i));
        }
        def rehashList(xs: List[Entry]): Unit = xs.match {
            case Nil => ()
            case e :: es => {
                val idx = hash(e.key) & (newSize - 1);
                newTable(idx) = e :: newTable(idx);
                rehashList(es);
            }
        }
        rehash(table.length - 1);
        table = newTable;
        threshold = ((initialSize as Float) * loadFactor) as Int;
    }

    private def hash(x: A) = {
		var h: Int = x.hashCode();
        h = h + ~(h << 9);
        h = h ^ (h >>> 14);
        h = h + (h << 4);
        h ^ (h >>> 10);
    }

    private def index(x: A) = hash(x) & (table.length - 1);
}
