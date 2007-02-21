package test;

import java.lang.annotation.*;

@Retention(value=RetentionPolicy.RUNTIME)
public @interface SourceAnnotation {
   public String url();
   public String mail();
}
