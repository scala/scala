public class Test {
  static class OuterBase implements OuterBaseInterface {
    static class StaticInner {}
    class Inner {}
  }
  interface OuterBaseInterface {
    interface InnerFromInterface {}
  }
  public static class Outer extends OuterBase {
    public static class Nested {
      public static P<StaticInner, Inner, InnerFromInterface> sig; // was: "type StaticInner", "not found: type Inner", "not found: type InnerFromInterface"
      public static P<Outer.StaticInner, Outer.Inner, Outer.InnerFromInterface> sig1; // was: "type StaticInner is not a member of Test.Outer"
      public static P<OuterBase.StaticInner, OuterBase.Inner, OuterBaseInterface.InnerFromInterface> sig2;

    }
    public class Nested1 {
      public P<StaticInner, Inner, InnerFromInterface> sig; // was: "not found: type StaticInner"
      public P<Outer.StaticInner, Outer.Inner, Outer.InnerFromInterface> sig1; // was: "type StaticInner is not a member of Test.Outer"
      public P<OuterBase.StaticInner, OuterBase.Inner, OuterBaseInterface.InnerFromInterface> sig2;
    }
  }
  public class Outer1 extends OuterBase {
    public class Nested1 {
      public P<StaticInner, Inner, InnerFromInterface> sig; // was: "not found: type StaticInner"
      public P<Outer.StaticInner, Outer.Inner, Outer.InnerFromInterface> sig1; // was: "type StaticInner is not a member of Test.Outer"
      public P<OuterBase.StaticInner, OuterBase.Inner, OuterBaseInterface.InnerFromInterface> sig2;
    }
  }
  public static class P<A, B, C>{}
}
