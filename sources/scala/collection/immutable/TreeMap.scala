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
 class TreeMap[KEY, VALUE](order: Order[KEY]) extends Tree[KEY, Pair[KEY, VALUE]](order)
                                              with Map[KEY, VALUE] {

   override type This = TreeMap[KEY, VALUE];

   /** A factory to create empty maps of the same type of keys.
    */
   def empty[C] = new TreeMap[KEY, C](order);

  /** Returns the key of an entry.
  *   This method has to be defined by concrete implementations
  *   of the class.
  */
   override def entryKey(entry:Pair[KEY,VALUE]) = entry._1;

   /** Creates a new TreeMap from a GBTree and its size. */
   protected def New(sz:Int,t:aNode):This =
     new TreeMap[KEY,VALUE](order) {
       override def size=sz;
       override protected val tree:aNode=t;
     }

   /**
   *   A new TreeMap with the entry added is returned,
   *   if key is <em>not</em> in the TreeMap, otherwise
   *   the key is updated with the new entry.
   */
   def update(key:KEY, value:VALUE) = update_or_add(key,Pair(key,value));

   /**
   *   A new TreeMap with the entry added is returned,
   *   assuming that key is <em>not</em> in the TreeMap.
   */
   def insert(key:KEY,value:VALUE) = add(key,Pair(key,value));

   /** Removes the key from the TreeMap.
   */
   def -(key:KEY) = delete_any(key);

   /** Check if this map maps <code>key</code> to a value and return the
   *  value if it exists.
   *
   *  @param  key     the key of the mapping of interest
   *  @return the value of the mapping, if it exists
   */
   override def get(key:KEY) =
     findValue(key).match {
       case Some(Pair(_,value:VALUE)) => Some(value);
       case _ => None;
     }

   /** Retrieve the value which is associated with the given key. This
   *  method throws an exception if there is no mapping from the given
   *  key to a value.
   *
   *  @param  key     the key
   *  @return the value associated with the given key.
   *  @throws Error("key not found").
   */
   override def apply(key:KEY):VALUE = tree.apply(key)._2;


   /** Creates a list of all (key, value) mappings.
   *
   *  @return    the list of all mappings
   */
   override def toList:List[Pair[KEY,VALUE]] = tree.toList(scala.Nil);

   /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
    def elements:Iterator[Pair[KEY,VALUE]] = entries;


   /** Compares two maps structurally; i.e. checks if all mappings
   *  contained in this map are also contained in the other map,
   *  and vice versa.
   *
   *  @return    true, iff both maps contain exactly the same mappings.
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

}
