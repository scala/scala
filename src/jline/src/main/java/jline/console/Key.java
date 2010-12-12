/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */

package jline.console;

import java.util.HashMap;
import java.util.Map;

/**
 * Map from key name to key codes.
 *
 * @author <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 * @author <a href="mailto:jason@planet57.com">Jason Dillon</a>
 * @see java.awt.event.KeyEvent
 * @since 2.0
 */
public enum Key
{
    CTRL_A(1),

    CTRL_B(2),

    CTRL_C(3),

    CTRL_D(4),

    CTRL_E(5),

    CTRL_F(6),

    CTRL_G(7),

    CTRL_K(11),

    CTRL_L(12),

    CTRL_N(14),

    CTRL_P(16),

    CTRL_OB(27),

    CTRL_QM(127),

    BACKSPACE('\b'),

    DELETE(127),;

    public final short code;

    Key(final int code) {
        this.code = (short) code;
    }

    private static final Map<Short, Key> codes;

    static {
        Map<Short, Key> map = new HashMap<Short, Key>();

        for (Key op : Key.values()) {
            map.put(op.code, op);
        }

        codes = map;
    }

    public static Key valueOf(final int code) {
        return codes.get((short) code);
    }
}