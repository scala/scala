class J { 
  S1 s1;
  S2 s2;
  
  String s = bar(S3.foo(), S3.bar("def"));
  
  private String bar(String s1, String s2) {
    return s1 + s2;
  }
}
