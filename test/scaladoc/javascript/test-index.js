Index.PACKAGES = { pkg1: [ { 'class': 'pkg1/Foo.html', name: 'pkg1.Foo' } ] };

asyncTest('Trac #4463 - kindFilter', function () {
    setTimeout(function () {
       start();

       equal($('#tpl ol.templates:visible').size(), 1);

       kindFilter('packs');
       equal($('#tpl ol.templates:visible').size(), 0);
       equal($('#tpl li.pack a.packhide').text(), 'show');

       kindFilter('all');
       equal($('#tpl ol.templates:visible').size(), 1);
       equal($('#tpl li.pack a.packhide').text(), 'hide');
    }, 1000);
});
