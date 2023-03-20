
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toMap;

public interface Identifiable_1 {
    String name();

    static <E extends Enum<E> & Identifiable_1> Map<String, E> valuesByName(final E[] values) {
        return Stream.of(values).collect(toMap(Identifiable_1::name, Function.identity()));
    }
}
