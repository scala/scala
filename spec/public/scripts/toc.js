/*!
 * toc - jQuery Table of Contents Plugin
 * v0.3.2
 * http://projects.jga.me/toc/
 * copyright Greg Allen 2014
 * MIT License
*/
(function($) {
var verboseIdCache = {};
$.fn.toc = function(options) {
  var self = this;
  var opts = $.extend({}, jQuery.fn.toc.defaults, options);

  var container = $(opts.container);
  var headings = $(opts.selectors, container);
  var headingOffsets = [];
  var activeClassName = opts.activeClass;

  var scrollTo = function(e, callback) {
    $('li', self).removeClass(activeClassName);
    $(e.target).parent().addClass(activeClassName);
  };

  //highlight on scroll
  var timeout;
  var highlightOnScroll = function(e) {
    if (timeout) {
      clearTimeout(timeout);
    }
    timeout = setTimeout(function() {
      var top = $(window).scrollTop(),
        highlighted, closest = Number.MAX_VALUE, index = 0;
      
      for (var i = 0, c = headingOffsets.length; i < c; i++) {
        var currentClosest = Math.abs(headingOffsets[i] - top);
        if (currentClosest < closest) {
          index = i;
          closest = currentClosest;
        }
      }
      
      $('li', self).removeClass(activeClassName);
      highlighted = $('li:eq('+ index +')', self).addClass(activeClassName);
      opts.onHighlight(highlighted);      
    }, 50);
  };
  if (opts.highlightOnScroll) {
    $(window).bind('scroll', highlightOnScroll);
    highlightOnScroll();
  }

  return this.each(function() {
    //build TOC
    var el = $(this);
    var ul = $(opts.listType);

    headings.each(function(i, heading) {
      var $h = $(heading);
      headingOffsets.push($h.offset().top - opts.highlightOffset);

      var anchorName = opts.anchorName(i, heading, opts.prefix);

      //add anchor
      if(heading.id !== anchorName) {
        var anchor = $('<span/>').attr('id', anchorName).insertBefore($h);
      }

      //build TOC item
      var a = $('<a/>')
        .text(opts.headerText(i, heading, $h))
        .attr('href', '#' + anchorName)
        .bind('click', function(e) {
          $(window).unbind('scroll', highlightOnScroll);
          scrollTo(e, function() {
            $(window).bind('scroll', highlightOnScroll);
          });
          el.trigger('selected', $(this).attr('href'));
        });

      var li = $('<li/>')
        .addClass(opts.itemClass(i, heading, $h, opts.prefix))
        .append(a);

      ul.append(li);
    });
    el.html(ul);
  });
};


jQuery.fn.toc.defaults = {
  container: 'body',
  listType: '<ul/>',
  selectors: 'h1,h2,h3',
  prefix: 'toc',
  activeClass: 'toc-active',
  onHighlight: function() {},
  highlightOnScroll: true,
  highlightOffset: 100,
  anchorName: function(i, heading, prefix) {
    if(heading.id.length) {
      return heading.id;
    }

    var candidateId = $(heading).text().replace(/[^a-z0-9]/ig, ' ').replace(/\s+/g, '-').toLowerCase();
    if (verboseIdCache[candidateId]) {
      var j = 2;
      
      while(verboseIdCache[candidateId + j]) {
        j++;
      }
      candidateId = candidateId + '-' + j;
      
    }
    verboseIdCache[candidateId] = true;

    return prefix + '-' + candidateId;
  },
  headerText: function(i, heading, $heading) {
    return $heading.text();
  },
  itemClass: function(i, heading, $heading, prefix) {
    return prefix + '-' + $heading[0].tagName.toLowerCase();
  }

};

})(jQuery);
