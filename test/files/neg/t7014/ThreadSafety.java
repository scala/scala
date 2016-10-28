package t7014;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME) // must be exactly RUNTIME retention (those we parse)
public @interface ThreadSafety {
  ThreadSafetyLevel level();
}