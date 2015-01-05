
public class Jamb_1 {
    // k(42, true, false) fails to compile without tupling
    public String k(Object a) { return "foo"; }
    public String k(int i, boolean b) { return "bar"; }
}
