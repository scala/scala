/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer.matching;

import scalac.ast.*;
import scalac.util.*;
import scalac.symtab.*;
import java.util.*;


public class TestRegTraverser extends Traverser {
    boolean result = false;
	Set variables = new HashSet();

    public void traverse(Tree tree) {
    	if (!result)
			switch (tree) {
				case Alternative(Tree[] ts):
					result = true;
					break;
				case Bind(_, Tree pat):
					variables.add(tree.symbol());
					traverse(pat);
					break;
				case Ident(_):
					if (variables.contains(tree.symbol()))
						result = true;
					break;
				default:
					super.traverse( tree );
			}
    }

    public static boolean apply(Tree t) {
        TestRegTraverser trt = new TestRegTraverser();
        trt.traverse(t);
        //System.err.println("TestRegTraverser says "+t+" -> "+trt.result);
        return trt.result;
    }
}
