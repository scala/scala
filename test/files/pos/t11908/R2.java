// javaVersion: 16+
final record R2(int i, String s) implements IntLike {
    public int getInt() {
        return i;
    }

    // Canonical constructor
    public R2(int i, String s) {
        this.i = i;
        this.s = s.intern();
    }
}
