// epishiny fixes the epicurve palette; recolour series by name after each render
(function () {
  var palette = {};
  function apply(id, tries) {
    var el = document.getElementById(id);
    var w = el && window.HTMLWidgets && HTMLWidgets.find("#" + id);
    var chart = w && w.getChart && w.getChart();
    if (!chart) {
      if (tries > 0) setTimeout(function () { apply(id, tries - 1); }, 150);
      return;
    }
    chart.series.forEach(function (s) {
      var col = palette[s.name];
      if (col && s.color !== col) s.update({ color: col }, false);
    });
    chart.redraw();
  }
  Shiny.addCustomMessageHandler("recolour", function (msg) {
    palette = msg.palette || {};
    apply(msg.id, 10);
  });
  $(document).on("shiny:value", function (e) {
    if (e.name === "curve-chart") setTimeout(function () { apply("curve-chart", 10); }, 200);
  });
})();
