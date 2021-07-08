// javaVersion: 16+
public record R3(int i, long l, String s) {

    // User-specified accessor
    public int i() {
        return i + 1; // evil >:)
    }

    // Not an accessor - too many parameters
    public long l(long a1, long a2) {
        return a1 + a2;
    }

    // Secondary constructor
    public R3(String s, int i) {
        this(i, 42L, s);
    }

    // Compact constructor
    public R3 {
        s = s.intern();
    }
}
