package scala.tools.testing;

import org.junit.ClassRule;
import org.junit.rules.TestRule;
import org.junit.runners.model.Statement;

import java.io.Closeable;
import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Extend this class to use JUnit's @ClassRule. This annotation only works on static methods,
 * which cannot be written in Scala.
 *
 * Example: {@link scala.tools.nsc.backend.jvm.opt.InlinerTest}
 */
public class ClearAfterClass {
    private static Map<Class<?>, Map<String, Object>> cache = new ConcurrentHashMap<>();

    @ClassRule
    public static TestRule clearClassCache() {
        return (statement, desc) -> new Statement() {
            @Override
            public void evaluate() throws Throwable {
                ConcurrentHashMap<String, Object> perClassCache = new ConcurrentHashMap<>();
                cache.put(desc.getTestClass(), perClassCache);
                try {
                    statement.evaluate();
                } finally {
                    perClassCache.values().forEach(ClearAfterClass::closeIfClosable);
                    cache.remove(desc.getTestClass());
                }
            }
        };
    }

    private static void closeIfClosable(Object o) {
        if (o instanceof Closeable) {
            try {
                ((Closeable) o).close();
            } catch (IOException e) {
                // ignore
            }
        }
    }

    @SuppressWarnings("unchecked")
    public <T> T cached(String key, scala.Function0<T> t) {
        Map<String, Object> perClassCache = cache.get(getClass());
        return (T) perClassCache.computeIfAbsent(key, s -> t.apply());
    }

}
