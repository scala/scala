package p;

// permitted subclasses in other compilation units and a nested class of another class
sealed interface Shape permits Circle, Holder.Inner, S {}
