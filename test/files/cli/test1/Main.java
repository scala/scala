// @info no dependency
package test1;
public class Main {
  public static void main(String args[]) {
    String arg = (args.length > 0) ? args[0] : "?";
    System.out.println("1: test " + arg + " passed (" + args.length + ")");
  }
}
