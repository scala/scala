
public enum JetBrains {
	APPLE {
		@Override public String text() {
			return "Cupertino tech company";
		}
	},
	ORANGE {
		@Override public String text() {
			return "SoCal county";
		}
	};
	public abstract String text();
}
