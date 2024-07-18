//> using jvm 15+
class Invalid2 {

    // Closing delimiter is first three eligible `"""`, not last
    public static final String closingDelimiterIsNotScalas = """
      foo"""";
}
