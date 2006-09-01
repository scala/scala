/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

import java.util.{Comparator, TreeSet}

class StateSetComparator extends Comparator {

  // use lexicographic order
  def compare(o1: Any, o2: Any): Int = {
    val it1 = o1.asInstanceOf[TreeSet].iterator()
    val it2 = o2.asInstanceOf[TreeSet].iterator()
    while (it1.hasNext()) {
      while (it2.hasNext()) {
        if (!it1.hasNext())
          return -1;

        val i1 = it1.next().asInstanceOf[Integer].intValue()
        val i2 = it2.next().asInstanceOf[Integer].intValue()
        if (i1 < i2)
          return -1;
        else if (i1 > i2)
          return 1;
      }
      if (it1.hasNext())
        return 1;
    }
    if (it2.hasNext()) -1 else 0
  }
}
