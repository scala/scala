/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;

class TreeMapFactory[KEY](order:Order[KEY]) extends MapFactory[KEY] {
  def Empty[VALUE] = new TreeMap[KEY,VALUE](order);
}

/** This class implements immutable maps using a tree.
 *
 *  @author  Erik Stenman, Matthias Zenger
 *  @version 1.0, 23/07/2003
 */
 class TreeMap[KEY,VALUE](order:Order[KEY]) extends Tree[KEY,Pair[KEY,VALUE]](order) with Map[KEY, VALUE] {
   override type This = TreeMap[KEY,VALUE];
   val factory = new TreeMapFactory[KEY](order);
   override def entryKey(entry:Pair[KEY,VALUE]) = entry._1;

   protected def New(sz:Int,t:aNode):This =
     new TreeMap[KEY,VALUE](order) {
       override def size=sz;
       override protected val tree:aNode=t;
     }

   def update(key:KEY, value:VALUE) = update_or_add(key,Pair(key,value));

   def insert(key:KEY,value:VALUE) = add(key,Pair(key,value));
   def -(key:KEY) = delete_any(key);



   /** Check if this map maps <code>key</code> to a value and return the
   *  value if it exists.
   *
   *  @param  key     the key of the mapping of interest
   *  @return the value of the mapping, if it exists
   */
   override def get(key:KEY) =
     find(key).match {
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

   def elements:Iterator[Pair[KEY,VALUE]] = entries;


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
