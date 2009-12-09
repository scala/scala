// © 2009 EPFL/LAMP
// written by Gilles Dubochet with contributions by Johannes Rudolph and "spiros"

$(document).ready(function(){
	cleanPackage($("#tpl"));
	$("#tpl ol > li span:contains('(class)')").replaceWith("<img class='icon' src='lib/class.png'/>");
	$("#tpl ol > li span:contains('(object)')").replaceWith("<img class='icon' src='lib/object.png'/>");
	$("#tpl ol > li span:contains('(trait)')").replaceWith("<img class='icon' src='lib/trait.png'/>");
	$("#tpl ol > li span:contains('(package)')").replaceWith("<img class='icon' src='lib/package.png'/>");
	$("#tpl a[href]").click(function(event){
		$("#content>iframe").attr("src", event.currentTarget.href);
		return false;
	});
	$("#quickflt").focus(function(event) {
		$("#quickflt").select();
	});
	function search() {
		var query = $("#quickflt").attr("value");
		// Regexp that matches CamelCase subbits: "BiSe" is
		// "[a-z]*Bi[a-z]*Se" and matches "BitSet", "ABitSet", ...
		var queryRegExp = new RegExp(query.replace(/([A-Z])/g,"[a-z]*$1"));
		$("#tpl ol.templates > li").each(function(){
			var item = $(this).attr("title");
			if (item == "" || queryRegExp.test(item)) {
				$(this).show();
				$(this).removeClass("hide");
			}
			else {
				$(this).addClass("hide");
				$(this).hide();
			};
		});
		cleanPackage($("#tpl"));
		pendingTimeout = undefined;
	};
	var pendingTimeout = undefined;
	$("#quickflt").bind("keyup", function(event) { 
    	if (event.keyCode == 27) { // escape 
 		    $("#quickflt").attr("value", "");
 		}
 		if (pendingTimeout != undefined) {
 			clearTimeout(pendingTimeout);
 		}
 		pendingTimeout = setTimeout(search, 200); //delay 0.2 sec
 	});
	$("#tpl .packages > li").prepend("<a class='packhide'>hide</a>");
	$("#tpl .packages > li > a.packhide").click(function(event){
		var action = $(this).text();
		if (action == "hide") {
			$("~ ol", $(this)).hide();
			$(this).text("show");
		}
		else {
			$("~ ol", $(this)).show();
			$(this).text("hide");
		}
		return false;
	});
});

/* Recusively go through the packages and show only those which contain visible items. */
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
	if ($("> ol.templates > li:not(.hide)", pack).length > 0) {
		$("> h3", pack).show();
		$("> .packhide", pack).show();
	}
	else {
		$("> h3", pack).hide();
		$("> .packhide", pack).hide();
	};
	return false;
}
