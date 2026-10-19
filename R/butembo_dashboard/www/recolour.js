// epishiny fixes the epicurve palette; recolour series by name after each render
(function () {
  var palette = {};
  var naCol = "#bdbdbd";
  // any no-value series name, whatever the grouping variable spells it
  var naRe = /^\(?\s*(unknown|inconnu|missing|manquant|not applicable|non applicable|n\/a|na)\s*\)?$/i;
  // only touches series whose colour differs, so the render hook below cannot loop
  function paint(chart) {
    var changed = false;
    chart.series.forEach(function (s) {
      var name = String(s.name).trim();
      var col = naRe.test(name) ? naCol : palette[name];
      if (col && s.color !== col) {
        s.update({ color: col }, false);
        changed = true;
      }
    });
    if (changed) chart.redraw();
  }
  function apply(id, tries) {
    hook();
    var el = document.getElementById(id);
    var w = el && window.HTMLWidgets && HTMLWidgets.find("#" + id);
    var chart = w && w.getChart && w.getChart();
    if (!chart) {
      if (tries > 0) setTimeout(function () { apply(id, tries - 1); }, 150);
      return;
    }
    paint(chart);
  }
  // epishiny updates series through a highcharter proxy on filter changes, which
  // fires neither shiny:value nor the recolour message; repaint after each render
  // this script loads before Highcharts, so register on first use rather than at load
  var hooked = false;
  function hook() {
    if (hooked || !window.Highcharts) return;
    hooked = true;
    // "render" also covers the first draw, which never fires "redraw"
    Highcharts.addEvent(Highcharts.Chart, "render", function () {
      if (this.renderTo && this.renderTo.closest("#curve-chart")) paint(this);
    });
  }
  Shiny.addCustomMessageHandler("recolour", function (msg) {
    palette = msg.palette || {};
    naCol = msg.na_col || naCol;
    apply(msg.id, 10);
  });
  $(document).on("shiny:value", function (e) {
    if (e.name === "curve-chart") setTimeout(function () { apply("curve-chart", 10); }, 200);
  });
})();
