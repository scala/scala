/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scalai;

public class EvaluatorResult {

    //########################################################################
    // Public Cases

    public case Void;
    public case Value(Object value, String type);
    public case Error(EvaluatorException exception);

    //########################################################################
}
