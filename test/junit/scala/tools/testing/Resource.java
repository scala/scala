
package scala.tools.testing;

import java.lang.annotation.*;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * An annotation for test scenarios, akin to common Resource.
 */
@Retention(RUNTIME)
public @interface Resource {
    Class<?> type();
}
