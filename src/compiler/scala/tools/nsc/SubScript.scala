/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import scala.collection.{ mutable, immutable }
import mutable.{ ListBuffer, StringBuilder }
import scala.reflect.internal.{ ModifierFlags => Flags }
import scala.reflect.internal.Chars.{ isScalaLetter }
import scala.reflect.internal.util.{ SourceFile, OffsetPosition }

trait SubScript extends scala.reflect.internal.Names {
  
  val global : Global
  import global._
  
  def underscore_name(name: Name) = newTermName("_"+name)    
}
//object SubScript extends SubScript