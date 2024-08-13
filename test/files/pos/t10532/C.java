
public class C {
    public long f(long x, Long y) { return x + y; }
    public long f(Long x, Long y) { return x + y; }

    public long g(long x, String y) { return x + Long.parseLong(y); }
    public long g(Long x, String y) { return x + Long.parseLong(y); }

    public long h(long x) { return x + 1; }
    public long h(Long x) { return x + 1; }
}
