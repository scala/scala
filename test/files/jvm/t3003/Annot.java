import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
public @interface Annot {
    Class<?> optionType();
}
