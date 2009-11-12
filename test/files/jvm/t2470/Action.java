import java.lang.annotation.*;

@Retention(value=RetentionPolicy.RUNTIME)
public @interface Action {
    Task.Scope block() default Task.Scope.ACTION;
}
