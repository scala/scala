package scala.runtime;

import java.lang.invoke.*;
import java.util.regex.Pattern;

public final class SymbolLiteral {
    private SymbolLiteral() {
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType,
                                     String value) throws Throwable {
        ClassLoader classLoader = lookup.lookupClass().getClassLoader();
        MethodType type = MethodType.fromMethodDescriptorString("(Ljava/lang/Object;)Ljava/lang/Object;", classLoader);
        Class<?> symbolClass = Class.forName("scala.Symbol", false, classLoader);
        MethodHandle factoryMethod = lookup.findStatic(symbolClass, "apply", type);
        Object symbolValue = factoryMethod.invokeWithArguments(value);
        return new ConstantCallSite(MethodHandles.constant(symbolClass, symbolValue));
    }
}
