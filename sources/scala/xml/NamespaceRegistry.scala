/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */
package scala.xml ;

import scala.collection.mutable.HashMap;
import scala.collection.immutable.TreeMap;

/** thread-safe storage of namespace URIs. */
object NamespaceRegistry {

  final var next = 0;
  /* hashmap from namespaces to ints */
  final var nmap: HashMap[String, Int] = new HashMap();
  final var dmap: TreeMap[Int,String] = new TreeMap();

  private def newCode( uri:String ):Int = {
    val d = next;
    next = next + 1;
    nmap( uri ) = d;
    dmap( d ) = uri;
    d
  }

  /* returns a code for the argument namespace uri. Registers it if needed */
  def getCode(uri: String):Int = synchronized {
    val c = nmap.get( uri );
    if( c.isEmpty ) newCode( uri ) else c.get
  }

  def getNamespace( i:Int ):Namespace = synchronized {
    Namespace( dmap( i ) );
  }

  /* empty namespace has code 0 */
  getCode("");

}
