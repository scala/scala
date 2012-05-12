import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
public @interface TestAnnotation {
  public enum TestEnumOne { A, B }
  public enum TestEnumTwo { C, D }

  public TestEnumOne one();
  public TestEnumTwo two();
  public String strVal();
}
