enum En {
  @Deprecated Um
}

@interface Ann {
  En value();
}

public class A {
  @Ann(En.Um)
  public static void te() { return; }
}
