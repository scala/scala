class J extends S {
  // These references all work in Javac because `object O { class I }` erases to `O$I`

  void select1(S1.Inner1 i) { new S1.Inner1(); }
  void ident(Inner i) {}

  void ident1(Inner1 i) {}
  void select(S.Inner i) { new S.Inner(); }
}
