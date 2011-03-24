import scala.collection.immutable.Vector;
import scala.collection.immutable.List;

public class fromjava {

    void main(String[] args, Vector<String> x) {
        Vector<String> y = x.take(2);
        String h = y.head();
        System.out.println(h);
    }
    void main(String[] args, List<String> x) {
        List<String> y = x.drop(2);
        String h = y.head();
        System.out.println(h);
    }
}