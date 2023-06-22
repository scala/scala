
public enum JetBrains {
	APPLE {
		@Override public String text() {
			return "Cupertino tech company";
		}
	},
	ORANGE
	;
	public String text() {
		return "Boring default";
	}
}
