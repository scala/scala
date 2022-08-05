// javaVersion: 17+

public sealed interface Nat permits Nat.Zero, Nat.Succ {
    public static final record Zero() implements Nat {}
    public static final record Succ(Nat pred) implements Nat {}
}
