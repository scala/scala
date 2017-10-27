import java.lang.annotation.*;

public @interface JAnn {
  int value();
  String a() default "";
  Class<?> b() default String.class;
  Object[] c() default {};
  SuppressWarnings d() default @SuppressWarnings({}); // nested annot
  RetentionPolicy e() default RetentionPolicy.SOURCE; // enum
}
