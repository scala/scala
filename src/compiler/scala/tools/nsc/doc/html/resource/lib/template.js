$(document).ready(function(){
	var prefilters = $("#ancestors > ol > li").filter(function(){
		var name = $(this).attr("name");
		return name == "scala.Any" || name == "scala.AnyRef";
	});
	prefilters.removeClass("in");
	prefilters.addClass("out");
	filterInherit();
	$("#ancestors > ol > li").click(function(event){
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