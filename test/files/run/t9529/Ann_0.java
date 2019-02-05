package anns;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Repeatable(Ann_0.Container.class)
public @interface Ann_0 {
    String name();
    String value();

    @Retention(RetentionPolicy.RUNTIME)
    public static @interface Container {
        public Ann_0[] value() default {};
    }
}