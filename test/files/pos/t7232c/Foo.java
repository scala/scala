package pack;

import java.util.List;

public class Foo {
    public static class List {
        public void isInnerList() {}
    }
	public static List innerList() { throw new Error(); }
}
