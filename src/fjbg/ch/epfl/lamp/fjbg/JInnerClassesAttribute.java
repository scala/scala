package ch.epfl.lamp.fjbg;

import java.io.DataOutputStream;
import java.io.IOException;
import java.util.*;

/**
 * InnerClasses attribute. See section 4.7.5 of the JVM
 * Specification.
 *
 * @author Iulian Dragos
 */
public class JInnerClassesAttribute extends JAttribute {
	/** InnerClass entries */
	private Map/*<InnerClassEntry>*/ entries = new LinkedHashMap();

	/** Constant pool of the current classfile. */
	private JConstantPool pool;

    public JInnerClassesAttribute(FJBGContext context,
            JClass clazz) {
    	super(context, clazz);
    	this.pool = clazz.pool;
    }

    public void addEntry(String inner, String outer, String name, int flags) {
    	Entry e = new Entry(inner, outer, name, flags);
    	if (entries.containsKey(inner)) {
    		Entry other = (Entry) entries.get(inner);
    		assert other.outerInfo == e.outerInfo && other.originalName == e.originalName && other.innerFlags == e.innerFlags
    			: inner + "already declared as " + other;
    	} else
    		entries.put(inner, e);
    }

    public String getName() { return "InnerClasses"; }

	protected int getSize() {
		return 2 + entries.size() * 8;
	}

	protected void writeContentsTo(DataOutputStream stream) throws IOException {
		stream.writeShort(entries.size());
		for (Iterator it = entries.values().iterator(); it.hasNext(); ) {
			Entry e = (Entry)it.next();
			stream.writeShort(e.innerInfo);
			stream.writeShort(e.outerInfo);
			stream.writeShort(e.originalName);
			stream.writeShort(e.innerFlags);
		}
	}

	/** An entry in the InnerClasses attribute, as defined by the JVM Spec. */
	private class Entry {
		/** CONSTANT_Class_info index in the pool for the inner class (mangled). */
		int innerInfo;

		/** CONSTANT_Class_info index in the pool for the outer class (mangled). */
		int outerInfo;

		/** CONSTANT_Utf8_info index in the pool for the original name of the inner class. */
		int originalName;

		/** Short int for modifier flags. */
		int innerFlags;

		public Entry(int iI, int oI, int oN, int f) {
			this.innerInfo = iI;
			this.outerInfo = oI;
			this.originalName = oN;
			this.innerFlags = f;
		}

		public Entry(String innerClass, String outerClass, String name, int flags) {
			this(pool.addClass(innerClass),pool.addClass(outerClass), pool.addUtf8(name), flags);
		}

		/** Two entries are equal if they refer to the same inner class.
		 *  innerInfo represents a unique name (mangled).
		 */
		public boolean equals(Object other) {
			if (other instanceof Entry) {
				Entry otherEntry = (Entry) other;
				return otherEntry.innerInfo == this.innerInfo;
			}
			return false;
		}
	}
}
