/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

import java.lang.Integer;

private[jcl] object Tests {

  def main(args : Array[String]) : Unit = {
    hashSet;
    treeSet;
    treeMap;
  }

  def treeSet : Unit = {
    val set = new TreeSet[String];
    set + "aaa" + "bbb" + "ccc" + "ddd" + "eee" + "fff";
    Console.println(set);
    val rset : SortedSet[String] = set.range("b", "e");
    Console.println(rset);
    rset + "bad";
    Console.println(rset);
    Console.println(set);
    val fset : SortedSet[String] = rset.projection.filter(_.endsWith("d"));
    Console.println(fset);
    fset += "cd";
    Console.println(set);
    //set.projection.map(_.length).retain(x => x == 3);
    Console.println(set);
    Console.println(rset);
    Console.println(fset);
  }

  def treeMap : Unit = {
    val map = new TreeMap[String,Integer];
    map + ("bb" -> 3) + ("cc" -> 4) + ("aa" -> 2) + ("dd" -> 5);
    //Console.println(map);
    val rmap : SortedMap[String,Integer] = map.range("b", "d");
    rmap + ("bad" -> 10);
    Console.println(rmap);
    //Console.println(map);
    val fmap : SortedMap[String,Integer] = rmap.projection.filterKeys(k => k.length == 2);
    Console.println(fmap);
  }

  def hashSet = {
    val set = new HashSet[String];
    set + "hello" + "world" + "you" + "to";
    Console.println(set);
    val fset = set.projection.filter(s => s.length <= 3);
    Console.println(fset);
    fset += "xxx";
    Console.println(set);
    try {
      fset += "xxxx";
      throw new Error;
    } catch {
      case e : IllegalArgumentException =>
      case _ => throw new Error;
    }
    //val mset : MutableIterable[Int] = set // set.projection.map(s => s.length);
    //Console.println(mset);
    //mset.retain(n => n < 5);
    Console.println(set);
    val set1 = new HashSet[String] + "1" + "2" + "3";
    set ++ (set1);
    Console.println(set);
    set.transform(s => "x_" + s);
    Console.println(set);
  }
}
