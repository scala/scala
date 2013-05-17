package foo;

// Originally composed to accommodate pull request feedback, this test has
// uncovered a handful of bugs in FromJavaClassCompleter, namely:
// * SI-7071 non-public ctors get lost
// * SI-7072 inner classes are read incorrectly

// I'm leaving the incorrect results of FromJavaClassCompleters in the check
// file, so that we get notified when something changes there.
// ^^^ It's not clear what those incorrect results were, but the fix for SI-7359
// (backport of fix for SI-6548) has probably resolved some of these. OP, please revisit this comment.

class PackagePrivateJavaClass {
  private int privateField = 0;
  protected int protectedField = 1;
  public int publicField = 2;

  private static int privateStaticField = 3;
  protected static int protectedStaticField = 4;
  public static int publicStaticField = 5;

  private void privateMethod() {}
  protected void protectedMethod() {}
  public void publicMethod() {}

  private static void privateStaticMethod() {}
  protected static void protectedStaticMethod() {}
  public static void publicStaticMethod() {}

  private PackagePrivateJavaClass() {}
  protected PackagePrivateJavaClass(int x) {}
  public PackagePrivateJavaClass(int x, int y) {}
}

public class JavaClass_1 {
  private class PrivateJavaClass {}
  private static class PrivateStaticJavaClass {}
  protected class ProtectedJavaClass {}
  private static class ProtectedStaticJavaClass {}
  public class PublicJavaClass {}
  public static class PublicStaticJavaClass {}
  private static int staticField = 0;
}