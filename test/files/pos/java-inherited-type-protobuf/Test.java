package example;

public class Test {
    
}

class GeneratedMessage extends AbstractMessage {
    GeneratedMessage(Builder<?> builder) {
    }

    public abstract static class Builder <BuilderType extends Builder>
      extends AbstractMessage.Builder<BuilderType> {}
}

class AbstractMessage extends AbstractMessageLite
                                      implements Message {
  public static abstract class Builder<BuilderType extends Builder>
      extends AbstractMessageLite.Builder<BuilderType>
      implements Message.Builder {}
}

class AbstractMessageLite implements MessageLite {
  public static abstract class Builder<BuilderType extends Builder>
      implements MessageLite.Builder {
    }

}

interface Message extends MessageLite, MessageOrBuilder {
    static interface Builder extends MessageLite.Builder, MessageOrBuilder {}
}

interface MessageLite extends MessageLiteOrBuilder {
  interface Builder extends MessageLiteOrBuilder, Cloneable {}
}

interface MessageLiteOrBuilder {}

interface MessageOrBuilder extends MessageLiteOrBuilder {}