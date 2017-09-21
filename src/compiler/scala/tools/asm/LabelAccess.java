package scala.tools.asm;

/**
 * Temporary class to allow access to the package-private status field of class Label.
 */
public class LabelAccess {
    public static boolean isLabelFlagSet(Label l, int f) {
        return (l.status & f) != 0;
    }

    public static void setLabelFlag(Label l, int f) {
        l.status |= f;
    }

    public static void clearLabelFlag(Label l, int f) {
        l.status &= ~f;
    }
}
