$(document).ready(function(){
	cleanPackage($("#tpl"));
	$("#tpl ol > li span.class").replaceWith("<img class='icon' src='lib/class.png'/>");
	$("#tpl ol > li span.object").replaceWith("<img class='icon' src='lib/object.png'/>");
	$("#tpl ol > li span.trait").replaceWith("<img class='icon' src='lib/trait.png'/>");
	$("#tpl ol > li span.package").replaceWith("<img class='icon' src='lib/package.png'/>");
	$("#tpl a.tplshow").click(function(event){
		$("#content>iframe").attr("src", event.currentTarget.href);
		return false;
	});
	$("#quickflt").focus(function(event) {
		$("#quickflt").select();
	});
	$("#quickflt").keyup(function(event) {
		var b = $("#quickflt").attr("value");
		$("#tpl ol.templates > li").each(function(){
			var a = $(this).attr("title");
			if (a == "" || a.indexOf(b) >= 0) {
				$(this).show();
				$(this).removeClass("hide");
			}
			else {
				$(this).addClass("hide");
				$(this).hide();
			};
		});
		cleanPackage($("#tpl"));
	});
});

function cleanPackage(pack) {
	$("> ol.packages > li", pack).each(function(){ cleanPackage($(this)); });
	if ($("> ol > li:not(.hide)", pack).length > 0) {
		pack.show();
		pack.removeClass("hide");
	}
	else {
		pack.addClass("hide");
		pack.hide();
	};
	if ($("> ol.templates > li:not(.hide)", pack).length > 0) { $("> h3", pack).show(); }
	else { $("> h3", pack).hide(); };
	return false;
}