/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Template.java,v 1.2 2002/06/28 17:23:59 paltherr Exp $
// $Id$

package scalai;

import scalac.util.Debug;

public class Template {

    //########################################################################
    // Public Cases

    public case Global(ScalaTemplate template);
    public case JavaClass(Class clasz);

    //########################################################################
    // Public Methods

    public String toString() {
        switch (this) {

        case Global(ScalaTemplate template):
            return "Global(" + template + ")";

        case JavaClass(Class clasz):
            return "JavaClass(" + clasz + ")";

        default:
            throw Debug.abort("unknown case", this);
        }
    }

    //########################################################################
}
