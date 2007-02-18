//package attributes;

import java.lang.annotation.*;

@Retention(value=RetentionPolicy.RUNTIME)
@interface Source {
   public String url();
   public String mail();
}
