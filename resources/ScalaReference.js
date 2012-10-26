$(function() {

	var popoutTOC = $('<div>');
	popoutTOC.attr('id', 'popoutTOC');
	
	var popoutTOChead = $('<span class="head">Jump to...</span>');
	popoutTOChead.appendTo(popoutTOC);
	
	var content = $('<div class="links">');
	$('#TOC > *').clone().appendTo(content);
	content.appendTo(popoutTOC);
	var enter = function() { content.css('display', 'block') };
	var exit = function() { content.css('display', 'none') };

	popoutTOC.hover(enter, exit);

	// disabled for now, until better styled
	// popoutTOC.appendTo($('body'));

})