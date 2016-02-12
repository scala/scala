package scala.runtime;


import java.lang.invoke.*;
import java.util.Arrays;
import java.util.HashMap;

public final class LambdaDeserialize {

    private MethodHandles.Lookup lookup;
    private final HashMap<String, MethodHandle> cache = new HashMap<>();
    private final LambdaDeserializer$ l = LambdaDeserializer$.MODULE$;

    private LambdaDeserialize(MethodHandles.Lookup lookup) {
        this.lookup = lookup;
    }

    public Object deserializeLambda(SerializedLambda serialized) {
        return l.deserializeLambda(lookup, cache, serialized);
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType) throws Throwable {
        MethodType type = MethodType.fromMethodDescriptorString("(Ljava/lang/invoke/SerializedLambda;)Ljava/lang/Object;", lookup.getClass().getClassLoader());
        MethodHandle deserializeLambda = lookup.findVirtual(LambdaDeserialize.class, "deserializeLambda", type);
        MethodHandle exact = deserializeLambda.bindTo(new LambdaDeserialize(lookup)).asType(invokedType);
        return new ConstantCallSite(exact);
    }
}
