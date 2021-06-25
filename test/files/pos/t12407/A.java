public class A {
  public interface I {
    I[] getArray();
  }

  public interface J extends I {
    @Override
    J[] getArray();
  }
}
