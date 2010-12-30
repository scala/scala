/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

/**
 * Labels which can be attached to instructions.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JLabel {
    public final static int UNDEFINED_ANCHOR = -1;
    protected int anchor = UNDEFINED_ANCHOR;

    public boolean isAnchored() { return anchor != UNDEFINED_ANCHOR; }

    public int getAnchor() {
        assert isAnchored();
        return anchor;
    }

    public void setAnchor(int anchor) {
        assert !isAnchored();
        this.anchor = anchor;
    }
}
