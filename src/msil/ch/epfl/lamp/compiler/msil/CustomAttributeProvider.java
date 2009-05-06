/*
 * System.Reflection-like API for access to .NET assemblies (DLL & EXE)
 */

// $Id$

package ch.epfl.lamp.compiler.msil;

import java.util.List;
import java.util.LinkedList;
import java.util.Iterator;

/**
 * @author Nikolay Mihaylov
 * @version 1.0
 */
public abstract class CustomAttributeProvider implements ICustomAttributeProvider {

    //##########################################################################

    protected List/*<Attribute>*/ custAttrs;
    private static final Object[] EMPTY = new Object[0];

    //TODO: take inherit into account
    public Object[] GetCustomAttributes(boolean inherit) {
	initAttributes(null);
	return custAttrs.size() == 0 ? EMPTY
            : custAttrs.toArray(new Attribute[custAttrs.size()]);
    }

    //TODO: take inherit into account
    public Object[] GetCustomAttributes(Type attributeType, boolean inherit) {
	initAttributes(attributeType);
        List tAttrs = null;
        if (constrType == attributeType)
            tAttrs = custAttrs;
        else {
            tAttrs = new LinkedList();
            for (Iterator attrs = custAttrs.iterator(); attrs.hasNext(); ) {
                Attribute a = (Attribute) attrs.next();
                if (a.GetType() == attributeType) tAttrs.add(a);
            }
        }
	return tAttrs.size() == 0 ? EMPTY
            : tAttrs.toArray(new Attribute[tAttrs.size()]);
    }

    //TODO: take inherit into account
    public boolean IsDefined(Type attributeType, boolean inherit) {
	initAttributes(attributeType);
        if (constrType == attributeType)
            return custAttrs.size() > 0;
	Iterator attrs = custAttrs.iterator();
	while (attrs.hasNext()) {
	    if (((Attribute)attrs.next()).GetType() == attributeType)
		return true;
	}
	return false;
// 	return inherit && (DeclaringClass.BaseType != null)
// 	    && DeclaringClass.BaseType.IsDefined(inherit);
    }

    protected void addCustomAttribute(ConstructorInfo constr, byte[] value) {
        Attribute attr = new Attribute(constr, value);
        assert constrType == null || constrType == attr.GetType();
        if (custAttrs == null)
            custAttrs = new LinkedList();
	custAttrs.add(attr);
    }

    private void initAttributes(Type atype) {
	if (custAttrs != null
            && (constrType == null || constrType == atype))
	    return;
	custAttrs = new LinkedList();
        constrType = atype;
	loadCustomAttributes(atype);
    }

    protected void loadCustomAttributes(Type atype) {}

    private Type constrType;
}
