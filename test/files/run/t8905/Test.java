import java.util.Comparator;

public class Test {
  private static class DoubleComparator implements Comparator<Double> {
    public int compare(Double o1, Double o2) {
      return o1.compareTo(o2);
    }
  }

  public static void main(String[] args) {
    DoubleRDD rdd = new DoubleRDD();
    RDDLike<Double> rddLike = rdd;

    // This call works fine:
    double rddLikeMax = rddLike.max(new DoubleComparator());
    // In Scala 2.10.4, this code compiles but this call fails at runtime:
    // java.lang.NoSuchMethodError: DoubleRDD.max(Ljava/util/Comparator;)Ljava/lang/Double;
    double rddMax = rdd.max(new DoubleComparator());
  }
}
