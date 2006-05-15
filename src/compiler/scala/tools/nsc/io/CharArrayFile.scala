/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2006, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$


package scala.tools.nsc.io;


/**
 * This class implements an abstract regular file backed by a
 * character array.
 */
class CharArrayFile(name: String, path: String, chars: Array[Char])
  extends VirtualFile(name, path)
{

  def this(name: String, chars: Array[Char]) = this(name, name, chars);

  /** Reads the content of this abstract file into a byte array. */
  override def read: Array[Byte] = {
    //Why was this marked as not an implementation?  It looks
    //great to me.  -Lex
    //Predef.error("!!! not yet implemented");
    new String(chars).getBytes(); // !!!
  }

}
