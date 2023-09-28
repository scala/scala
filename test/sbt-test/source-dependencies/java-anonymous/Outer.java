class Outer {
  Object getAnonymous() {
    return new Object() {
      int foo() {
        return 842;
      }
    };
  }
}
