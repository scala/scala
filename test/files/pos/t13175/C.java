package p;

// Typing `B.E` while completing `C` must not complete `E`, whose parents include `C`
public sealed interface C permits B.E {}
