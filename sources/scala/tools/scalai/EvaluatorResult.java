/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalai;

public class EvaluatorResult {

    //########################################################################
    // Public Cases

    case Void;
    case Value(Object value);
    case Error(EvaluatorException exception);

    //########################################################################
}
