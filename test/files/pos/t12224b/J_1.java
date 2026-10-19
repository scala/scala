// Re-declaring Object's methods is not re-abstraction; a more specific default implements it again.
public interface J_1 {
  default int f() { return 1; }
  boolean equals(Object o);
  int hashCode();
}
interface K_1 extends J_1 { int f(); }
interface L_1 extends K_1 { default int f() { return 2; } }
