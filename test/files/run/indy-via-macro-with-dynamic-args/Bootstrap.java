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
    	MethodHandle Pattern_matcher = MethodHandles.lookup().findVirtual(java.util.regex.Pattern.class, "matcher", MethodType.fromMethodDescriptorString("(Ljava/lang/CharSequence;)Ljava/util/regex/Matcher;", lookup.lookupClass().getClassLoader()));
        return new ConstantCallSite(Pattern_matcher.bindTo(Pattern.compile(value)));
    }
}
