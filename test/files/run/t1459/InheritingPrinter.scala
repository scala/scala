class InheritingPrinter extends JavaPrinter {
  override def doit(s: String*) {
    print("InheritingPrinter extends ")
    super.doit(s: _*);
  }
}