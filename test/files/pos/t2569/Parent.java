package varargs;

  public class Parent {

      public String concatenate(String... strings) {
          StringBuilder builder = new StringBuilder();
          for (String s : strings) {
              builder.append(s);
          }
          return builder.toString();
      }

  }
