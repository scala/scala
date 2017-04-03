class C$D { public int i() { return 1; } }
class C$E { public int i() { return 1; } }
class C$F$G { public int i() { return 1; } }

// Test1 has a reference to C$D, which is a top-level class in this case,
// so there's no INNERCLASS attribute in Test1
class Test_1 {
  static C$D mD(C$D cd) { return cd; }
  static C$E mE(C$E ce) { return ce; }
  static C$F$G mG(C$F$G cg ) { return cg; }
}
