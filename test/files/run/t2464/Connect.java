package test;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Connect {

    LoadStyle loadStyle() default LoadStyle.EAGER;

    public enum LoadStyle {
        EAGER,
        DEFERRED,
        LAZY
    }
}
