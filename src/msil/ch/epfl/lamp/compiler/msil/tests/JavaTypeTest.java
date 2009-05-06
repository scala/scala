// $Id$

package ch.epfl.lamp.compiler.msil.tests;

import ch.epfl.lamp.compiler.msil.*;
import ch.epfl.lamp.compiler.msil.util.VJSAssembly;

public class JavaTypeTest {

    public static void main(String[] args) {
	if (args.length < 1) {
	    System.err.println("usage: java test.JavaTypeTest classname");
	    System.exit(1);
	}

	Type type = VJSAssembly.VJSLIB.GetType(args[0]);
	MembersTest.dumpType(System.out, type);
    }
}
