$(document).ready(function(){
	var prefilters = $("#ancestors > ol > li").filter(function(){
		var name = $(this).attr("name");
		return name == "scala.Any" || name == "scala.AnyRef";
	});
	prefilters.removeClass("in");
	prefilters.addClass("out");
	filterInherit();
	$("#ancestors > ol > li").click(function(){
		if ($(this).hasClass("in")) {
			$(this).removeClass("in");
			$(this).addClass("out");
		}
		else if ($(this).hasClass("out")) {
			$(this).removeClass("out");
			$(this).addClass("in");
		};
		filterInherit();
	});
	$(".signature .symbol .extype").hover(
		function(){
			var full = $(this).attr("name");
			var short = $(this).text();
			$(this).attr("name", short);
			$(this).text(full);
		},
		function(){
			var short = $(this).attr("name");
			var full = $(this).text();
			$(this).attr("name", full);
			$(this).text(short);
		}
	);
	$("#template div.fullcomment").hide();
	var docAllSigs = $("#template .signature");
	function commentShowFct(fullComment){
		var vis = $(":visible", fullComment);
		if (vis.length > 0) {
			fullComment.slideUp(100);
		}
		else {
			fullComment.slideDown(100);
		}
	};
	var docShowSigs = docAllSigs.filter(function(){
		return $("+ div.fullcomment", $(this)).length > 0;
	});
	docShowSigs.css("cursor", "help");
	docShowSigs.click(function(){
		commentShowFct($("+ div.fullcomment", $(this)));
	});
	function commentToggleFct(shortComment){
		var vis = $("~ div.fullcomment:visible", shortComment);
		if (vis.length > 0) {
			shortComment.slideDown(100);
			vis.slideUp(100);
		}
		else {
			var hid = $("~ div.fullcomment:hidden", shortComment);
			hid.slideDown(100);
			shortComment.slideUp(100);
		}
	};
	var docToggleSigs = docAllSigs.filter(function(){
		return $("+ p.shortcomment", $(this)).length > 0;
	});
	docToggleSigs.css("cursor", "help");
	docToggleSigs.click(function(){
		commentToggleFct($("+ p.shortcomment", $(this)));
	});
	$("p.shortcomment").click(function(){
		commentToggleFct($(this));
	});
});

function filterInherit() {
	$("#mbrsel > div > ol > li.in").each(function(){
		findMembersByOwner($(this).attr("name")).show();
	});
	$("#mbrsel > div > ol > li.out").each(function(){
		findMembersByOwner($(this).attr("name")).hide();
	});
	return false;
};

function findMembersByOwner(owner0) {
	return $(".members > ol > li").filter(function(){
		var qualName1 = $(this).attr("name");
		if (qualName1 == undefined) return false;
		return owner0 == qualName1.slice(0, qualName1.indexOf("#"));
	});
};

function findMemberByName(name0) {
	return $(".members > ol > li").filter(function(){
		var qualName1 = $(this).attr("name");
		if (qualName1 == undefined) return false;
		return name0 == qualName1.slice(qualName1.indexOf("#") + 1);
	}).eq(0);
};