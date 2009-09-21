// $Id$

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.IOException;

/**
 * Java class field.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JField extends JFieldOrMethod {
    protected JField(FJBGContext context,
                     JClass owner,
                     int accessFlags,
                     String name,
                     JType type) {
        super(context, owner, accessFlags, name, type);
    }

    protected JField(FJBGContext context,
                     JClass owner,
                     DataInputStream stream)
        throws IOException {
        super(context, owner, stream);
    }
}
