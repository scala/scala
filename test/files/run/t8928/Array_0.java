package test;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
//TODO: make this work
//@java.lang.annotation.Repeatable(Array_0.Repeated.class)
public @interface Array_0 {
    int[] value();

    @Retention(RetentionPolicy.RUNTIME)
    public @interface Repeated {
        Array_0[] value();
    }
}