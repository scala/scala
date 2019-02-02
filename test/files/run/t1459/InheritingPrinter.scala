class InheritingPrinter extends JavaPrinter {
  override def doit(s: String*): Unit = {
    print("InheritingPrinter extends ")
    super.doit(s: _*);
  }
}
