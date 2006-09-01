/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

import java.util.{HashMap, TreeSet}

/** cartesian
 */

/** Int x TreeSet[ Int ]
 */
case class Npair(nstate: Integer, nset: TreeSet) {

  override def equals(that: Any): Boolean = this match {
    case Npair(nstate, nset) =>
      that match {
        case Npair(_nstate, _nset) =>
          (nstate == _nstate) && (nset == _nset)
        case _ =>
          false
      }
    case _ =>
      false
  }

  override def toString(): String = this match {
    case Npair(nstate, nset) =>
      //Integer dstate = (Integer) indexMap.get(nset);
      "<n" + nstate.toString() + " in " + nset /*+" = d"+dstate*/ + ">";
    case _ =>
      null
  }

  def toString(indexMap: HashMap): String = {
    //assert indexMap != null
    this match {
      case Npair(nstate, nset) =>
        //assert nstate != null
        val dstate = indexMap.get( nset ).asInstanceOf[Integer]
        "<n" + nstate.toString() + " in " + nset + " = d" + dstate + ">"
      case _ =>
        null
    }
  }

}

class NpairComparator extends StateSetComparator {
  override def compare(o1: Any, o2: Any): Int = o1 match {
    case Npair(nstate, nset) => o2 match {
      case Npair(_nstate, _nset) =>
        val res = nstate.compareTo(_nstate)
        if (res != 0) res else super.compare(nset, _nset)
    }
  }
}
