package scala.tools.testing;

import org.junit.AfterClass;

/**
 * Extend this class to use JUnit's @AfterClass. This annotation only works on static methods,
 * which cannot be written in Scala.
 *
 * Example: {@link scala.tools.nsc.backend.jvm.opt.InlinerTest}
 */
public class ClearAfterClass {
    public static interface Clearable {
        void clear();
    }

    public static Clearable stateToClear;

    @AfterClass
    public static void clearState() { stateToClear.clear(); }
}
