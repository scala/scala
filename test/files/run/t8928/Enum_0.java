package test;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
public @interface Enum_0 {
    Enum choice();

    public enum Enum {
        ONE, OTHER
    }
}