package lib;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE, ElementType.METHOD, ElementType.CONSTRUCTOR, ElementType.ANNOTATION_TYPE,
    ElementType.PACKAGE, ElementType.FIELD, ElementType.LOCAL_VARIABLE })
@Inherited
public @interface SomeAnnotation {
  String value();
  int year() default 2024;
  Class<?>[] classes() default {int.class, String.class};
}
