/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scala.tools.scalac.transformer.matching ;

import java.util.{ HashMap, TreeSet };
/** cartesian
 */

/** Int x TreeSet[ Int ]
 */
case class Npair(nstate: Integer, nset: TreeSet) {

  override def equals(that: Any): Boolean = {
    this match {
      case Npair(nstate, nset) =>
	that match {
	  case Npair(_nstate, _nset) =>
	    return ((nstate == _nstate)
		    && (nset == _nset));
          case _ => return false
	}
      case _ => return false
    }
  }

  override def toString(): String = this match {
    case Npair(nstate, nset) =>
      //Integer dstate = (Integer) indexMap.get(nset);
      "<n" + nstate.toString() + " in " + nset /*+" = d"+dstate*/ + ">";
    case _ => null
  }

 def toString(indexMap: HashMap):  String = {
    //assert indexMap != null;
    this match {
      case Npair(nstate, nset) =>
      //assert nstate != null;
      val dstate = indexMap.get( nset ).asInstanceOf[Integer];
      return "<n" + nstate.toString() + " in " + nset + " = d" + dstate + ">";
      case _ =>
        return null;
    }
  }


}

class NpairComparator extends StateSetComparator {
  override def compare(o1: Any, o2: Any): Int = {
    o1 match {
      case Npair(nstate, nset) => o2 match {
	case Npair(_nstate, _nset) =>
	  val res = nstate.compareTo(_nstate);
          if (res != 0)
	    return res;
	  else
	    return super.compare(nset, _nset);
      }
    }
  }
}
