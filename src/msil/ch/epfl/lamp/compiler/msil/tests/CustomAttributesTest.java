
package ch.epfl.lamp.compiler.msil.tests;

import ch.epfl.lamp.compiler.msil.*;
import ch.epfl.lamp.compiler.msil.util.Table;

import java.io.PrintStream;

public class CustomAttributesTest {
    public static void main(String[] args) {
	if (args.length < 1) {
	    System.err.println("You must supply a filename!");
	    System.exit(1);
	}

	Assembly assem = Assembly.LoadFrom(args[0]);
	Type.initMSCORLIB(assem);

	testCustomAttributes();
    }

    public static void testCustomAttributes() {
	Object[] attrs = Type.GetType("System.ObsoleteAttribute")
	    .GetCustomAttributes(false);
	assert attrs != null;
	for (int i = 0; i < attrs.length; i++) {
	    System.out.println("\t" + attrs[i]);
	}
    }

}
