/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

/**
 * Representation of a local variable or method argument.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class JLocalVariable {
    protected final JMethod owner;
    protected final JType type;
    protected final String name;
    protected final int index;

    protected JLocalVariable(FJBGContext context,
                             JMethod owner,
                             JType type,
                             String name,
                             int index) {
        this.owner = owner;
        this.type = type;
        this.name = name;
        this.index = index;

        assert index < 0xFFFF : "index too big for local variable: " + index;
    }

    public JMethod getOwner() { return owner; }
    public int getIndex() { return index; }
    public String getName() { return name; }
    public JType getType() { return type; }

    /*@Override*/ public String toString() {
        return "0\t"+type.getSize()+"\t"+index+"\t"+name+"\t"+type;
    }
}
