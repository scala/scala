import scala.concurrent.duration.Duration;
import java.util.*;
import java.util.concurrent.TimeUnit;
import static java.util.concurrent.TimeUnit.*;

public class Test {
  public static List<Double> inputs = Arrays.asList(0d, 1d, 7d, 10d, 12d, 24d, 30d, 60d, 100d, 1000d, 1e6);
  public static List<Double> makeNumbers() {
    ArrayList<Double> xs = new ArrayList<Double>();
    for (Double n1: inputs) {
      for (Double n2: inputs) {
        Double n = n1 * n2;
        if (!xs.contains(n))
          xs.add(n);
      }
    }
    Double[] arr = xs.toArray(new Double[0]);
    Arrays.sort(arr);
    return Arrays.asList(arr);
  }

  public static void p(Object x) {
    System.out.println(x);
  }
  public static void main(String[] args) {
    for (TimeUnit t : TimeUnit.values()) {
      for (Double n: makeNumbers()) {
        String s = "" + n + " " + t.toString().toLowerCase();
        String result;
        try {
          Duration d = Duration.create(n, t);
          result = d.toString();
        } catch(Exception e) {
          result = e.getClass().toString();
        }
        p(String.format("%25s  =>  %s", s, result));
      }
    }
    for (String s: new String[] {"10000000000000001 nanoseconds", "10000000000000002 nanoseconds"})
      p(String.format("%25s  =>  %s", s, Duration.create(s)));
    for (String s: Arrays.asList("Inf", "-Inf", "+Inf", "PlusInf", "MinusInf")) {
      Duration d = Duration.create(s);
      p(String.format("%25s  =>  %s", s, d));
    }
  }
}
