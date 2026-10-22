// epishiny fixes the epicurve palette; recolour series by name after each render
(function () {
  var palette = {};
  var naCol = "#bdbdbd";
  // any no-value series name, whatever the grouping variable spells it
  var naRe = /^\(?\s*(unknown|inconnu|missing|manquant|not applicable|non applicable|n\/a|na)\s*\)?$/i;
  // epishiny's shared tooltip has no total and shows a raw date for weekly bars
  function patchTooltip(chart) {
    if (chart._tipPatched) return;
    chart._tipPatched = true;
    chart.update({
      tooltip: {
        shared: true,
        formatter: function () {
          var iv = Shiny.shinyapp && Shiny.shinyapp.$inputValues["curve-date_interval"];
          var head = String(iv).toLowerCase() === "week"
            ? "S" + new Date(this.x).getWeek()
            : Highcharts.dateFormat("%e %b %Y", this.x);
          var total = 0;
          var rows = this.points.map(function (p) {
            total += p.y;
            var pct = p.percentage !== undefined && this.points.length > 1
              ? " (" + Highcharts.numberFormat(p.percentage, 1) + "%)"
              : "";
            return '<span style="color:' + p.color + '">●</span> ' + p.series.name +
              ": <b>" + p.y + pct + "</b><br/>";
          }, this);
          return "<b>" + head + "</b><br/>" + rows.join("") +
            "<b>Total : " + Highcharts.numberFormat(total, 0) + "</b>";
        }
      }
    }, false);
  }
  // only touches series whose colour differs, so the render hook below cannot loop
  function paint(chart) {
    patchTooltip(chart);
    var changed = false;
    chart.series.forEach(function (s) {
      var name = String(s.name).trim();
      var col = naRe.test(name) ? naCol : palette[name];
      if (col && s.color !== col) {
        s.update({ color: col }, false);
        changed = true;
      }
    });
    if (fadeLatestWeek(chart)) changed = true;
    if (changed) chart.redraw();
  }
  // the latest data week is still filling; key off the data, as Sys.Date() lags it
  function fadeLatestWeek(chart) {
    var maxX = -Infinity;
    chart.series.forEach(function (s) {
      s.points.forEach(function (p) { if (p.x > maxX) maxX = p.x; });
    });
    if (!isFinite(maxX)) return false;
    var dow = (new Date(maxX).getUTCDay() + 6) % 7;
    var weekStart = maxX - dow * 864e5;
    var changed = false;
    chart.series.forEach(function (s) {
      var faded = Highcharts.color(s.color).setOpacity(0.35).get("rgba");
      s.points.forEach(function (p) {
        if (p.x >= weekStart && p.color !== faded) {
          p.update({ color: faded }, false);
          changed = true;
        }
      });
    });
    return changed;
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
