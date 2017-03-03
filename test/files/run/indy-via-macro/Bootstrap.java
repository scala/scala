package test;

import java.lang.invoke.*;
import java.util.regex.Pattern;

public final class Bootstrap {
    private Bootstrap() {
    }

    /** Pre-compile a regex */
    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType,
                                     String value) throws Throwable {
        return new ConstantCallSite(MethodHandles.constant(Pattern.class, Pattern.compile(value)));
    }
}
