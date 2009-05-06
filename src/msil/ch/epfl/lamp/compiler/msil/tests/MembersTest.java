// $Id$

package ch.epfl.lamp.compiler.msil.tests;

import ch.epfl.lamp.compiler.msil.*;
import ch.epfl.lamp.compiler.msil.util.Table;

import java.io.PrintStream;

public class MembersTest {

    public static void main(String[] args) {
	if (args.length < 1) {
	    System.err.println
		("usage: java test.MembersTest assembly [classname]");
	    System.exit(1);
	}

	Assembly mscorlib = Assembly.LoadFrom("mscorlib.dll");
	Type.initMSCORLIB(mscorlib);
	Assembly assem = Assembly.LoadFrom(args[0]);
	if (args.length > 1) {
	    Type type = assem.GetType(args[1]);
	    if (type != null)
		dumpMember(System.out, type);
	    else System.err.println("Cannot find type " + args[1]
				    + " in " + assem);
	} else {
	    Type[] types = assem.GetTypes();
	    System.out.println("Number of types in assembly " + assem
			       + " -> " + types.length);
            dumpCustomAttributes(System.out, "assembly: ", assem);
            Module[] modules = assem.GetModules();
            for (int i = 0; i < modules.length; i++) {
                dumpCustomAttributes(System.out, "module " + modules[i] + ": ",
                                     modules[i]);
            }
            dumpMembers(System.out, types);
	}
    }

    public static final void dumpMember(PrintStream out, MemberInfo member) {
        try {
            if (member.MemberType() == MemberTypes.TypeInfo
                || member.MemberType() == MemberTypes.NestedType) {
                Type type = (Type)member;
                dumpCustomAttributes(out, "", type);
                out.print(TypeAttributes.accessModsToString(type.Attributes));
                out.print(type.IsInterface() ? " interface " : " class ");
                out.print(type);
                if (type.BaseType() != null)
                    out.println(" extends " + type.BaseType());
                Type[] ifaces = type.GetInterfaces();
                if (ifaces.length > 0) {
                    out.print("\timplements ");
                    for (int i = 0; i < ifaces.length; i++) {
                        out.print(ifaces[i]);
                        if (i < (ifaces.length - 1))
                            out.print(", ");
                    }
                    out.println();
                }
                out.println("{");
                int all = BindingFlags.Public | BindingFlags.DeclaredOnly// | BindingFlags.NonPublic
                    | BindingFlags.Instance | BindingFlags.Static;
                dumpMembers(out, type.GetNestedTypes());
                dumpMembers(out, type.GetFields(all));
                dumpMembers(out, type.GetConstructors(all));
                dumpMembers(out, type.GetMethods(all));
                dumpMembers(out, type.GetProperties(all));
                dumpMembers(out, type.GetEvents());
                out.println("}");
            } else {
                dumpCustomAttributes(out, "", member);
                out.print(MemberTypes.toString(member.MemberType()));
                out.print(": "); out.print(member);
                out.println();
            }
        } catch (Throwable e) {
            String message = MemberTypes.toString(member.MemberType())
                + ": " + member;
            throw new RuntimeException(message, e);
        }
    }

    public static void dumpCustomAttributes(PrintStream out,
                                            String prefix,
                                            ICustomAttributeProvider att)
    {
        Object[] attrs = att.GetCustomAttributes(false);
        for (int j = 0; j < attrs.length; j++)
            out.println(prefix + attrs[j]);
    }

    public static void dumpMembers(PrintStream out, MemberInfo[] members) {
	for (int i = 0; i < members.length; i++) {
            dumpMember(out, members[i]);
	}
    }

}
