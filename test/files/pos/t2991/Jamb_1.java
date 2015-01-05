
public class Jamb_1 {
    // ambiguous to Scala, not to Java
    public String f(Object a) { return "foo"; }
    public String f(Object a, Object... as) { return "bar"; }

    // ditto
    public <T> T g(T t) { return t; }
    public <T> T g(T t, T... ts) { return ts[0]; }

    // even more ambiguous
    public <T> T j(T t) { return t; }
    public <T> T j(T t, T u ) { return u; }
    public <T> T j(T t, T u, T... ts) { return ts[0]; }
}
