/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;

/** This class implements immutable maps using a tree.
 *
 *  @author  Erik Stenman, Matthias Zenger
 *  @version 1.0, 23/07/2003
 */
class TreeMap[KEY,VALUE](order:Order[KEY]) extends
    Map[KEY, VALUE, TreeMap[KEY,VALUE]]
    with Tree[KEY,Pair[KEY,VALUE]](order,
	{(entry:Pair[KEY,VALUE]) =>
	    entry.match {case Pair(key,_value)=> key:KEY}
	}) {

    private def mkTreeMap(sz:int,t:aNode):TreeMap[KEY,VALUE] =
	new TreeMap[KEY,VALUE](order){
	    override def size=sz;
	    override protected val tree:aNode=t;
	};


    def update(key:KEY, value:VALUE):TreeMap[KEY,VALUE] = {
	if(contains(key)) mkTreeMap(size,tree.update(key,Pair(key,value)))
	else insert(key,value);
    }

    def insert(key:KEY,value:VALUE) = {
	val newSize = size+1;
	mkTreeMap(newSize,tree.insert(key, Pair(key,value),
				    pow(newSize, p)).node);
    }


    /** Check if this map maps <code>key</code> to a value and return the
     *  value if it exists.
     *
     *  @param  key     the key of the mapping of interest
     *  @return the value of the mapping, if it exists
     */
     def get(key:KEY) =
	tree.get(key).match {
	    case Some(Pair(_,value:VALUE)) => Some(value);
	    case _ => None;
	}


    def -(key:KEY) = delete_any(key);

    def delete_any(key:KEY) =
	if(contains(key)) delete(key)
			      else this;

    // delete. Assumes that key is present.
    def delete(key:KEY) =
	mkTreeMap(size - 1, tree.delete(key));

    /** Retrieve the value which is associated with the given key. This
     *  method throws an exception if there is no mapping from the given
     *  key to a value.
     *
     *  @param  key     the key
     *  @return the value associated with the given key.
     *  @throws Error("key not found").
     */
    override def apply(key:KEY):VALUE = tree.apply(key)._2;

    /** Is the given key mapped to a value by this map?
     *
     *  @param   key        the key
     *  @return true, iff there is a mapping for key in this map
     */
    override def contains(key:KEY) = tree.is_defined(key);

    /** Creates a list of all (key, value) mappings.
     *
     *  @return    the list of all mappings
     */
    override def toList:List[Pair[KEY,VALUE]] = tree.toList(scala.Nil);

    def elements:Iterator[Pair[KEY,VALUE]] = entries;
}
