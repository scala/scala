package test;

import java.lang.annotation.Annotation;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(value=RetentionPolicy.RUNTIME)
public @interface SourceAnnotation_1 {
  public String value();

  public String[] mails() default {"bill.gates@bloodsuckers.com"};
}
