package pack;

import java.util.List;   // does shadow
import java.awt.*;       // doesn't shadow, keep looking
import java.util.function.Supplier;  // irrelevant, keep looking

public class Foo {
    // should be java.util.List.
    public static List list() { throw new Error(); }
}
