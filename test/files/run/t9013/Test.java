import java.util.Comparator;

public class Test {
  public static void main(String[] args) {
    ClassImplementsClass c = new ClassImplementsClass();

    c.x("a", "b", "c");
    c.y("a", "b", "c");
    c.z("a", "b", "c");

    VarargAbstractClass i = new ClassImplementsClass();

    i.x("a", "b", "c");
    i.y("a", "b", "c");
    // System.out.println(i.z("a", "b", "c")); // still incurs a LinkageError.
    // Perhaps due to Uncurry:
    //   > for every repeated Java parameter `x: T...' --> x: Array[T], except:
    //   >    if T is an unbounded abstract type, replace --> x: Array[Object]
  }
}
