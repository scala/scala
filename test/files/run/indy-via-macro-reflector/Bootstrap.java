package test;

import java.lang.invoke.*;

public final class Bootstrap {
    private Bootstrap() {
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType,
                                     Object... args) throws Throwable {
        int arity = (int) args[0];
        MethodHandle MH = (MethodHandle) args[1];
        String[] strings = new String[arity];
        for (int i = 0; i < arity; i++) {
            strings[i] = (String) args[2 + i];
        }

        Reflection handleAndStrings = new Reflection(MH, strings);
        MethodHandle foo = MethodHandles.lookup().findVirtual(Reflection.class, "foo", MethodType.methodType(String.class, String.class));
        return new java.lang.invoke.ConstantCallSite(foo.bindTo(handleAndStrings));
    }
    static class Reflection {
        private final MethodHandle handle;
        private final String[] scalaParamNames;

        public Reflection(MethodHandle handle, String[] scalaParamNames) {
            this.handle = handle;
            this.scalaParamNames = scalaParamNames;
        }

        public String foo(String f) {
            return toString() + ", " + f;
        }

        @java.lang.Override
        public java.lang.String toString() {
            return "HandleAndStrings{" +
                "handle=" + handle +
                ", scalaParamNames=" + java.util.Arrays.toString(scalaParamNames) +
                '}';
        }
    }
}
