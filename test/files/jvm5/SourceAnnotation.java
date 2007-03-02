package test;

import java.lang.annotation.*;

@Retention(value=RetentionPolicy.RUNTIME)
public @interface SourceAnnotation {
   public String value();
   public String mail() default "bill.gates@bloodsuckers.com";
}
