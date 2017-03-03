public class Q {

  public static class Builder {}

  public static class Inner {
    public static class Builder { public void innerMethod() {} }
    public Builder foo() { return new Builder(); } // this line gives an error, that Builder is ambiguous

    public Inner.Builder viaSelect() { return new Builder(); } // this line gives an error, that Builder is ambiguous
  }

}

