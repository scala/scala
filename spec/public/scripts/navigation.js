(function($){ $.fn.navigation = function() {

  // the TOC already contains H1 so we start at H2
  var headers = $('h2, h3, h4, h5').filter(function() {
    // exclude examples
    if (this.id.substr(0, 7) == 'example') {
      return false;
    }

    // get all headers with an id
    return this.id;
  });

  var output = $(this);

  var get_level = function(n) { return parseInt(n.nodeName.replace('H', ''), 10); }

  var back_to_top = '<span title="Return to top" class="to_top octicon octicon-chevron-up"></span>';

  if (headers.length && output.length) {
    var level = get_level(headers[0]);
    var current_level;
    var html = '<ol>';

    headers.each(function(_, header) {
      current_level = get_level(header);

      if (current_level === level) {
        // same level as before
        html += '<li><a href="#' + header.id + '">' + header.innerHTML + '</a>';
      } else if (current_level <= level) {
        // higher level, we go back up and chose intermediary lists
        for(i = current_level; i < level; i++) {
          html += '</li></ol>';
        }
        html += '<li><a href="#' + header.id + '">' + header.innerHTML + '</a>';
      } else if (current_level > level) {
        // lower level, we open new nested lists
        for(i = current_level; i > level; i--) {
          html += '<ol><li>';
        }
        html += '<a href="#' + header.id + '">' + header.innerHTML + '</a>';
      }

      var header_link = '<a class="anchor" href="#' + header.id + '"><span class="octicon octicon-link"></span></a>';
      $(header).prepend(header_link);

      if (!$(header).prev().is('h1')) {
        $(header).after(back_to_top);
      }

      level = current_level;
    });

    html += '</ol>';

    output.html(html);
  }

  // back to top links
  $(document).on('click', '.to_top', function() {
    $(window).scrollTop(0);
    window.location.hash = '';
  });

  // we add one more at the end of the document
  $('#content-container').append(back_to_top);

};})(jQuery);

