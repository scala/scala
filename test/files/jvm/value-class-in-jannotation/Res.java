
package res;

import java.lang.annotation.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Retention(RUNTIME)
public @interface Res {
    Class<?> type();
}
