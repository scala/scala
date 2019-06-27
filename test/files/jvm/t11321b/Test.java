import t11321.*;

public class Test {
    public static void main(String ...args) {
        scala.Option<X> b = new Foo().b();
        System.out.println(b.get().x());
        System.out.println(b.get().x().getClass());
    }
}