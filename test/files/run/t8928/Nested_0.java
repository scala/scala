package test;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
public @interface Nested_0 {
    Inner inner();

    @Retention(RetentionPolicy.RUNTIME)
    public @interface Inner {
        String value();
    }
}