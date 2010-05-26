
package ch.epfl.lamp.fjbg;

/**
 * Representation of a local variable or method argument.
 *
 * @version 1.0
 * @author Michel Schinz
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
}
