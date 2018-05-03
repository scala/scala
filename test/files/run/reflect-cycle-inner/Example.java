package example;

import java.util.Collections;
import java.util.Iterator;

public class Example implements Iterable<Example.Inner> {
    public static class Inner {

    }

    public Iterator<Inner> iterator() {
        return Collections.emptyIterator();
    }
}
