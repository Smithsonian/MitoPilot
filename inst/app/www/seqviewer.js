// inst/app/www/seqviewer.js
// Sequence viewer for the annotation details window (tools/nt_viewer_spec.md).
(function () {
  'use strict';

  // ---- geometry: pure functions, testable without a canvas ----
  function span(f, len) {
    return f.pos1 <= f.pos2 ? f.pos2 - f.pos1 + 1 : len - f.pos1 + 1 + f.pos2;
  }
  function wraps(f) { return f.pos1 > f.pos2; }
  // Greedy interval packing on linearised coordinates; a wrapped feature also
  // claims [1, pos2], so it is checked against both ends of the circle.
  function lanes(feats, len) {
    var order = feats.slice().sort(function (a, b) { return a.pos1 - b.pos1; });
    var laneEnds = [];   // last linearised end per lane
    var laneHeads = [];  // last [1, pos2] head per lane (wrapped features)
    order.forEach(function (f) {
      var end = f.pos1 + span(f, len) - 1;
      var head = wraps(f) ? f.pos2 : 0;
      var lane = -1;
      for (var i = 0; i < laneEnds.length; i++) {
        var free = laneEnds[i] < f.pos1 && laneHeads[i] < f.pos1;
        if (free && head > 0) {
          // the tail [1, head] must not touch anything already in this lane
          var clash = order.some(function (g) {
            return g.lane === i && g !== f && g.pos1 <= head;
          });
          free = !clash;
        }
        if (free) { lane = i; break; }
      }
      if (lane < 0) { lane = laneEnds.length; laneEnds.push(0); laneHeads.push(0); }
      laneEnds[lane] = Math.max(laneEnds[lane], end);
      laneHeads[lane] = Math.max(laneHeads[lane], head);
      f.lane = lane;
    });
    return laneEnds.length;
  }
  function nCodons(f, len) { return Math.floor(span(f, len) / 3); }
  // 1-based position of the middle base of codon i (0-based).
  function codonCentre(f, i, len, topology) {
    var p = f.dir === '-' ? f.pos2 - 3 * i - 1 : f.pos1 + 3 * i + 1;
    if (topology === 'circular') p = ((p - 1) % len + len) % len + 1;
    return p;
  }
  // Letter for codon i: the stored translation, "*" for a trailing stop codon
  // the translation does not include, nothing otherwise.
  function stopLetter(f, i, len) {
    var tr = f.translation || '';
    if (i < tr.length) return tr.charAt(i);
    var n = nCodons(f, len);
    return (i === n - 1 && tr.length === n - 1) ? '*' : '';
  }

  window.mpseq = window.mpseq || {};
  window.mpseq.geom = { span: span, wraps: wraps, lanes: lanes, nCodons: nCodons,
                        codonCentre: codonCentre, stopLetter: stopLetter };
})();
