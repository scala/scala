package anns;

import java.lang.annotation.*;

@Repeatable(TypeAnn_0.Anns.class)
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE_USE)
public @interface TypeAnn_0 {
    String value();

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.TYPE_USE)
    public static @interface Anns {
        TypeAnn_0[] value();
    }
}