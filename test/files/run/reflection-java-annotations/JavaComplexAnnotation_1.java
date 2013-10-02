import java.lang.annotation.Target;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD, ElementType.METHOD, ElementType.TYPE})
public @interface JavaComplexAnnotation_1 {
  byte v1();
  short v2();
  char v3();
  int v4();
  long v5();
  float v6();
  double v7();
  boolean v8() default false;
  // void v9();
  String v10();
  Class<?> v11();
  JavaSimpleEnumeration_1 v12();
  JavaSimpleAnnotation_1 v13();
  byte[] v101();
  short[] v102();
  char[] v103();
  int[] v104();
  long[] v105();
  float[] v106();
  double[] v107();
  boolean[] v108();
  String[] v110();
  Class<?>[] v111();
  JavaSimpleEnumeration_1[] v112();
  JavaSimpleAnnotation_1[] v113();
}