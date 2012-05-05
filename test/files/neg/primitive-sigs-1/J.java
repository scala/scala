// java: often the java or scala compiler will save us from
// the untruth in the signature, but not always.
public class J {
  public static Integer f(AC<Integer> x) { return x.f(); }
  public static void main(String[] args) {
    f(new Bippy());
  }
}
