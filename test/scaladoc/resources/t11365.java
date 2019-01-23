/**
 * A package header
 */
package test.scaladoc;

/**
 * Testing java comments don't flag Scala specific errors
 */
public class JavaComments {
	static @interface Annot {
	}

	private class Route {}
 	final java.util.List<Route> routes = null;

	abstract class AnnotImpl implements Annot {}

}
