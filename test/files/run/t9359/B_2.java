public class B_2 {
  // nested final
  public enum A1N_FINAL {
    A1N_FINAL_VAL
  }

  // nested, non-final
  public enum A1N {
    A1N_VAL { } // value has a body, so a class extending A1N is generated
  }

  // nested, non-final, abstract
  public enum A1N_ABSTRACT {
    A1N_ABSTRACT_VAL {
      void foo() { return; }
    };
    abstract void foo(); // abstract member makes the enum class abstract
  }
}
