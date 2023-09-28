package typeparameters;

import java.util.function.Supplier;

public class Example {

  static <I, O> void call() {
    Supplier<BaseBlah<I, O>> blah = () ->
      new BaseBlah<I, O>() {
        @Override
        protected O getResponseInternal(I i) {
          return null;
        }
      };
  }

  public static void main(String[] args) {
    Example.<String, String>call();
  }
}

abstract class BaseBlah<I, O> {
  protected O getResponseInternal(I i) {
    return null;
  }
}