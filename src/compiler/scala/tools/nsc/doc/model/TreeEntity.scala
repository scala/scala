


/** we create a TreeEntity to store some text and some hyperLinks to make on entities in it */

package scala.tools.nsc
package doc
package model

import scala.collection.immutable.TreeMap


class TreeEntity {
  var expression:String = ""
  var refs = new TreeMap[Int, (Entity, Int)] // start, (Entity to be linked to , end)
}