package test;

import java.lang.invoke.*;

public final class Bootstrap {
    private Bootstrap() {
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType,
                                     MethodType mt) throws Throwable {
        return new java.lang.invoke.ConstantCallSite(java.lang.invoke.MethodHandles.constant(MethodType.class, mt));
    }
}
