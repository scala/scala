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
abstract class HashTable[A] {

    /** The load factor for the hash table.
     */
    protected val loadFactor: Float = 0.75f;

    /** The initial size of the hash table.
     */
    protected val initialSize: Int = 16;

    /** The initial threshold
     */
    protected val initialThreshold: Int = ((initialSize as Float) * loadFactor) as Int;

    /** The actual hash table.
     */
    protected var table: Array[List[Entry]] = new Array(initialSize);
    initTable(initialSize - 1);

    /** The number of mappings contained in this hash table.
     */
    protected var tableSize: Int = 0;

    /** The next size value at which to resize (capacity * load factor).
     */
    protected var threshold: Int = initialThreshold;

    /** Returns the size of this hash map.
     */
    def size = tableSize;

    protected def findEntry(key: A): Option[Entry] =
        table(index(elemHashCode(key))).find(entryFor(key));

    protected def addEntry(e: Entry): Unit = {
        val h = index(elemHashCode(entryKey(e)));
        table(h) = e :: table(h);
        tableSize = tableSize + 1;
        if (tableSize > threshold)
            resize(2 * table.length);
    }

    def remove(key: A): Unit = {
        val old = findEntry(key);
        old match {
            case None =>
            case Some(e) => {
                val idx = index(elemHashCode(key));
                table(idx) = table(idx).filter(e => !elemEquals(entryKey(e), key));
                tableSize = tableSize - 1;
            }
        }
    }

    def clear = {
        initTable(table.length - 1);
        tableSize = 0;
    }

    protected type Entry;

    protected def entryKey(e: Entry): A;

    protected def entries: Iterator[Entry] = new Iterator[Entry] {
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

    private def entryFor(key: A) = (e: Entry => elemEquals(entryKey(e), key));

    private def initTable(n: Int): Unit = {
        table(n) = Nil;
        if (n > 0) initTable(n - 1);
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
                val idx = improve(elemHashCode(entryKey(e))) & (newSize - 1);
                newTable(idx) = e :: newTable(idx);
                rehashList(es);
            }
        }
        rehash(table.length - 1);
        table = newTable;
        threshold = ((newSize as Float) * loadFactor) as Int;
    }

    protected def elemEquals(key1: A, key2: A): Boolean = (key1 == key2);

    protected def elemHashCode(key: A) = key.hashCode();

    protected final def improve(hcode: Int) = {
        var h: Int = hcode + ~(hcode << 9);
        h = h ^ (h >>> 14);
        h = h + (h << 4);
        h ^ (h >>> 10);
    }

    protected final def index(hcode: Int) = improve(hcode) & (table.length - 1);
}
