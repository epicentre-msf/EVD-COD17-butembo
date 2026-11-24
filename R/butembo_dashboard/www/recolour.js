// Shared tooltip header for weekly bars (x = ISO Monday, UTC ms):
// "S12" on the first line, "16/03 - 22/03" below. Used by every weekly epicurve.
window.epiWeekHeader = function (x) {
  var d = new Date(x);
  // ISO week number: shift to the Thursday of the same week, count from 1 Jan
  var thu = new Date(Date.UTC(d.getUTCFullYear(), d.getUTCMonth(), d.getUTCDate()));
  thu.setUTCDate(thu.getUTCDate() + 3 - ((thu.getUTCDay() + 6) % 7));
  var jan1 = Date.UTC(thu.getUTCFullYear(), 0, 1);
  var week = Math.ceil(((thu - jan1) / 864e5 + 1) / 7);
  return "<b>S" + week + "</b><br/>" +
    '<span style="font-size:0.85em">' +
    Highcharts.dateFormat("%d/%m", x) + " - " +
    Highcharts.dateFormat("%d/%m", x + 6 * 864e5) + "</span><br/>";
};

// epishiny fixes the epicurve palette; recolour series by name after each render
(function () {
  // one palette per chart output id, since several epicurves can be on screen
  var palettes = {};
  var chartIds = ["curve-chart", "chains_summary-curve-chart"];
  var naCol = "#bdbdbd";
  // any no-value series name, whatever the grouping variable spells it
  var naRe = /^\(?\s*(unknown|inconnu|missing|manquant|not applicable|non applicable|n\/a|na)\s*\)?$/i;
  // epishiny's shared tooltip has no total and shows a raw date for weekly bars
  function patchTooltip(chart, id) {
    if (chart._tipPatched) return;
    chart._tipPatched = true;
    chart.update({
      tooltip: {
        shared: true,
        formatter: function () {
          var iv = Shiny.shinyapp && Shiny.shinyapp.$inputValues[id.replace(/-chart$/, "-date_interval")];
          var head = String(iv).toLowerCase() === "week"
            ? window.epiWeekHeader(this.x)
            : "<b>" + Highcharts.dateFormat("%e %b %Y", this.x) + "</b><br/>";
          var total = 0;
          var rows = this.points.map(function (p) {
            total += p.y;
            var pct = p.percentage !== undefined && this.points.length > 1
              ? " (" + Highcharts.numberFormat(p.percentage, 1) + "%)"
              : "";
            return '<span style="color:' + p.color + '">●</span> ' + p.series.name +
              ": <b>" + p.y + pct + "</b><br/>";
          }, this);
          return head + rows.join("") +
            "<b>Total : " + Highcharts.numberFormat(total, 0) + "</b>";
        }
      }
    }, false);
  }
  // only touches series whose colour differs, so the render hook below cannot loop
  function paint(chart, id) {
    var palette = palettes[id] || {};
    patchTooltip(chart, id);
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
  // the calendar week in progress has an incomplete count
  function fadeLatestWeek(chart) {
    var now = new Date();
    var today = Date.UTC(now.getFullYear(), now.getMonth(), now.getDate());
    var dow = (new Date(today).getUTCDay() + 6) % 7;
    var weekStart = today - dow * 864e5;
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
    paint(chart, id);
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
      var host = this.renderTo && this.renderTo.closest("[id$='-chart']");
      if (host && chartIds.indexOf(host.id) > -1) paint(this, host.id);
    });
  }
  Shiny.addCustomMessageHandler("recolour", function (msg) {
    palettes[msg.id] = msg.palette || {};
    naCol = msg.na_col || naCol;
    apply(msg.id, 10);
  });
  $(document).on("shiny:value", function (e) {
    if (chartIds.indexOf(e.name) > -1) setTimeout(function () { apply(e.name, 10); }, 200);
  });
})();
