package scalac.symtab;

import scalac.atree.AConstant;
import scalac.util.Debug;

public class AttributeInfo {

    public final Symbol constr;
    public final AConstant[] args;
    public final AttributeInfo next;

    public AttributeInfo(Symbol constr, AConstant[] args, AttributeInfo next) {
        this.constr = constr;
        this.args = args;
        this.next = next;
        assert constr.isConstructor() : Debug.show(constr);
    }

    public AConstant[] getAttrArguments(Symbol sym) {
        for (AttributeInfo attr = this; attr != null; attr = attr.next)
            if (attr.constr == sym)
                return attr.args;
        return null;
    }

    public String toString() {
        StringBuffer str = new StringBuffer();
        for (AttributeInfo attr = this; attr != null; attr = attr.next) {
            str.append('['); str.append(Debug.show(attr.constr.constructorClass()));
            str.append('(');
            for (int i = 0; i < attr.args.length; i++) {
                if (i > 0) str.append(", ");
                str.append(attr.args[i]);
            }
            str.append(")]\n");
        }
        return str.toString();
    }
}
