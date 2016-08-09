public class Q {

  public static class Builder {}

  public static class Inner {
    public static class Builder {}
    public Builder foo() { return new Builder(); } // this line gives an error, that Builder is ambiguous
  }

}
