/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.util.Iterator;
import scalac.symtab.Type;
import scalac.Global;

public interface TypeIsomorphism {

    Iterator/*[Pair[Symbol, Type]]*/ searchType(Type type, SymbolBooleanFunction isDocumented);

}
