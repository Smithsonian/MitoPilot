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

  // Genomic side made partial by strand + partial5/partial3: 'start' (pos1
  // end), 'end' (pos2 end), 'both', or null when both ends are real.
  function partialEdge(f) {
    var startOpen = f.dir === '-' ? f.partial3 : f.partial5;
    var endOpen = f.dir === '-' ? f.partial5 : f.partial3;
    if (startOpen && endOpen) return 'both';
    if (startOpen) return 'start';
    if (endOpen) return 'end';
    return null;
  }
  // Whether a linearised [a, b, k] piece shows the pos1 ("start") and/or
  // pos2 ("end") boundary at its own true edge, vs. being cut off by the
  // visible linearised range [vs, ve).
  function segOwns(seg, vs, ve) {
    return { start: seg[0] >= vs, end: seg[1] <= ve };
  }

  window.mpseq = window.mpseq || {};
  window.mpseq.geom = { span: span, wraps: wraps, lanes: lanes, nCodons: nCodons,
                        codonCentre: codonCentre, stopLetter: stopLetter,
                        partialEdge: partialEdge, segOwns: segOwns };
})();

(function () {
  'use strict';
  var G = window.mpseq.geom;
  var MAX_PPB = 14, NT_LETTER = 8, NT_BAR = 3, AA_MIN = 4;
  var RULER_H = 22, LANE_H = 22, NT_H = 20, AA_H = 20, GUTTER = 60, PAD = 4, COV_H = 60, ERR_H = 36;
  var ERR_FLAG = 0.05;
  // Same shades as the BLAST synteny zoom (app_annotate_details.R base_color)
  // and msaR's zappo scheme in the alignment viewer.
  var BASE = { A: '#4faf45', C: '#e0a53f', G: '#e0555a', T: '#4a90d9', N: '#666666' };
  var ZAPPO = { I: '#ffafaf', L: '#ffafaf', V: '#ffafaf', A: '#ffafaf', M: '#ffafaf',
                F: '#ffc800', W: '#ffc800', Y: '#ffc800', K: '#6464ff', R: '#6464ff', H: '#6464ff',
                D: '#ff0000', E: '#ff0000', S: '#00ff00', T: '#00ff00', N: '#00ff00', Q: '#00ff00',
                P: '#ff00ff', G: '#ff00ff', C: '#ffff00' };
  var viewers = {};

  function cssVar(name, fallback) {
    var v = getComputedStyle(document.documentElement).getPropertyValue(name).trim();
    return v || fallback;
  }
  function typeColor(t) { return cssVar('--mp-type-' + String(t || '').toLowerCase(), '#888888'); }
  function niceStep(raw) {
    var p = Math.pow(10, Math.floor(Math.log10(raw)));
    var m = raw / p;
    return (m <= 1 ? 1 : m <= 2 ? 2 : m <= 5 ? 5 : 10) * p;
  }

  function Viewer(id) {
    this.id = id;
    this.canvas = document.getElementById(id);
    this.wrap = this.canvas.parentElement;
    this.tip = document.getElementById(id.replace(/-canvas$/, '-tip'));
    this.section = this.canvas.closest('details');
    this.len = 0; this.seq = ''; this.feats = []; this.topology = 'linear';
    this.version = null; this.selected = null; this.nLanes = 0;
    this.viewStart = 1; this.ppb = 1;
    this.showNt = true; this.showAa = true; this.showCov = true; this.showErr = true;
    this.depth = null; this.err = null; this.depthMax = 0; this.errMax = 0;
    this.hits = []; this.aaRows = [];
    this.bindControls();
    var self = this;
    if (window.ResizeObserver) new ResizeObserver(function () { self.draw(); }).observe(this.wrap);
    if (this.section) this.section.addEventListener('toggle', function () { self.draw(); });
  }

  Viewer.prototype.load = function (p) {
    var sameSeq = (this.version === p.version && this.len === p.len);
    this.unit = p.unit; this.len = p.len; this.seq = p.seq || ''; this.topology = p.topology;
    this.version = p.version; this.input = p.input;
    this.feats = (p.features || []).map(function (f) { return Object.assign({}, f); });
    this.nLanes = G.lanes(this.feats, this.len);
    this.depth = p.depth || null; this.err = p.err || null;
    var mx = function (a, floor) { return (a || []).reduce(function (m, d) { return d !== null && d > m ? d : m; }, floor); };
    this.depthMax = mx(this.depth, 1); this.errMax = mx(this.err, 0);
    this.selected = (p.selected === undefined || p.selected === null) ? null : p.selected;
    if (!sameSeq) this.whole();
    this.draw();
  };

  // ---- view state ----
  Viewer.prototype.width = function () { return Math.max(200, this.wrap.clientWidth - GUTTER); };
  Viewer.prototype.minPpb = function () { return this.width() / this.len; };
  Viewer.prototype.viewLen = function () { return this.width() / this.ppb; };
  Viewer.prototype.clamp = function () {
    this.ppb = Math.min(MAX_PPB, Math.max(this.minPpb(), this.ppb));
    if (this.topology === 'linear') {
      this.viewStart = Math.max(1, Math.min(this.len - this.viewLen() + 1, this.viewStart));
    } else {
      this.viewStart = ((this.viewStart - 1) % this.len + this.len) % this.len + 1;
    }
  };
  Viewer.prototype.whole = function () { this.ppb = this.minPpb(); this.viewStart = 1; this.clamp(); this.draw(); };
  Viewer.prototype.zoom = function (factor, atPos) {
    var centre = atPos || (this.viewStart + this.viewLen() / 2);
    var newPpb = Math.min(MAX_PPB, Math.max(this.minPpb(), this.ppb * factor));
    this.viewStart = centre - (centre - this.viewStart) * (this.ppb / newPpb);
    this.ppb = newPpb; this.clamp(); this.draw();
  };
  Viewer.prototype.goto = function (pos) {
    if (this.ppb < NT_LETTER) this.ppb = 10;
    this.viewStart = pos - this.viewLen() / 2; this.clamp(); this.draw();
  };
  Viewer.prototype.fit = function (row) {
    var f = this.feats.find(function (x) { return x.row === row; });
    if (!f) return;
    this.selected = row;
    var s = G.span(f, this.len), margin = Math.max(3, s * 0.05);
    this.ppb = Math.min(MAX_PPB, this.width() / (s + 2 * margin));
    this.viewStart = f.pos1 - margin; this.clamp();
    if (this.section && !this.section.open) this.section.open = true;
    this.draw();
  };

  // ---- coordinate helpers (linearised: a position may exceed len when wrapped) ----
  Viewer.prototype.x = function (lin) { return GUTTER + (lin - this.viewStart) * this.ppb; };
  Viewer.prototype.segments = function (f) {
    var s = G.span(f, this.len), out = [], vs = this.viewStart, ve = this.viewStart + this.viewLen();
    var ks = this.topology === 'circular' ? [-1, 0, 1] : [0];
    ks.forEach(function (k) {
      var a = f.pos1 + k * this.len, b = a + s - 1;
      if (b >= vs && a <= ve) out.push([a, b, k]);
    }, this);
    return out;
  };
  Viewer.prototype.baseAt = function (lin) {
    lin = Math.floor(lin);
    var p = this.topology === 'circular' ? ((lin - 1) % this.len + this.len) % this.len + 1 : lin;
    if (p < 1 || p > this.len) return null;
    return { pos: p, base: this.seq.charAt(p - 1) || 'N' };
  };

  // ---- drawing ----
  Viewer.prototype.covOn = function () { return this.showCov && !!this.depth; };
  Viewer.prototype.errOn = function () { return this.showErr && !!this.err; };
  Viewer.prototype.topH = function () {
    return RULER_H + PAD + (this.covOn() ? COV_H + PAD : 0) + (this.errOn() ? ERR_H + PAD : 0);
  };
  Viewer.prototype.laneY = function (lane) { return this.topH() + lane * LANE_H; };
  Viewer.prototype.height = function () {
    var pcgs = this.showAa && this.ppb >= AA_MIN ? this.aaRows.length : 0;
    return this.topH() + this.nLanes * LANE_H + PAD + (this.showNt && this.ppb >= NT_BAR ? NT_H : 0) + pcgs * AA_H + PAD;
  };
  Viewer.prototype.draw = function () {
    if (!this.len || !this.canvas.offsetParent) return;
    this.clamp();
    var dpr = window.devicePixelRatio || 1, W = this.wrap.clientWidth;
    this.aaRows = this.showAa && this.ppb >= AA_MIN
      ? this.feats.filter(function (f) { return f.type === 'PCG' && f.translation !== undefined && this.segments(f).length; }, this)
      : [];
    var H = this.height();
    this.canvas.width = W * dpr; this.canvas.height = H * dpr;
    this.canvas.style.height = H + 'px';
    var c = this.canvas.getContext('2d'); c.setTransform(dpr, 0, 0, dpr, 0, 0);
    c.clearRect(0, 0, W, H);
    c.font = '12px ' + cssVar('--mp-font-mono', 'monospace');
    this.hits = [];
    this.drawRuler(c, W);
    var y = RULER_H + PAD;
    if (this.covOn()) { this.drawTrack(c, W, y, COV_H, this.depth, this.depthMax, 'depth', false); y += COV_H + PAD; }
    if (this.errOn()) { this.drawTrack(c, W, y, ERR_H, this.err, Math.max(this.errMax, ERR_FLAG * 2), 'error', true); y += ERR_H + PAD; }
    this.drawJoins(c); this.drawLanes(c);
    y = this.topH() + this.nLanes * LANE_H + PAD;
    if (this.showNt && this.ppb >= NT_BAR) { this.drawNt(c, y); y += NT_H; }
    this.aaRows.forEach(function (f) { this.drawAa(c, f, y); y += AA_H; }, this);
  };
  Viewer.prototype.drawRuler = function (c, W) {
    var step = niceStep(90 / this.ppb), vs = this.viewStart, ve = vs + this.viewLen();
    c.fillStyle = cssVar('--mp-text-muted', '#6a6a6a'); c.strokeStyle = cssVar('--mp-border', '#ccc');
    c.textAlign = 'center'; c.textBaseline = 'top';
    for (var lin = Math.ceil(vs / step) * step; lin <= ve; lin += step) {
      var b = this.baseAt(lin); if (!b) continue;
      var x = this.x(lin);
      c.beginPath(); c.moveTo(x, RULER_H - 6); c.lineTo(x, RULER_H); c.stroke();
      c.fillText(String(b.pos), x, 2);
    }
    if (this.topology === 'circular') {
      [0, 1].forEach(function (k) {
        var lin = 1 + k * this.len;
        if (lin >= vs && lin <= ve) {
          var x = this.x(lin); c.save(); c.strokeStyle = cssVar('--mp-primary', '#337ab7'); c.setLineDash([3, 3]);
          c.beginPath(); c.moveTo(x, 0); c.lineTo(x, this.height()); c.stroke(); c.restore();
        }
      }, this);
    }
  };
  // Connector between the pieces of a joined feature (notes JOIN: marker).
  Viewer.prototype.drawJoins = function (c) {
    var groups = {}, W = this.wrap.clientWidth;
    this.feats.forEach(function (f) { if (f.joined) (groups[f.joined] = groups[f.joined] || []).push(f); });
    c.lineWidth = 1; c.setLineDash([]);
    Object.keys(groups).forEach(function (k) {
      var g = groups[k].sort(function (a, b) { return a.pos1 - b.pos1; });
      for (var i = 1; i < g.length; i++) {
        var x0 = this.x(g[i - 1].pos2 + 1), x1 = this.x(g[i].pos1);
        if (x1 < GUTTER || x0 > W) continue;
        c.strokeStyle = typeColor(g[i].type);
        c.beginPath();
        c.moveTo(Math.max(GUTTER, x0), this.laneY(g[i - 1].lane) + (LANE_H - 6) / 2);
        c.lineTo(Math.min(W, x1), this.laneY(g[i].lane) + (LANE_H - 6) / 2);
        c.stroke();
      }
    }, this);
  };
  Viewer.prototype.drawLanes = function (c) {
    var vs = this.viewStart, ve = this.viewStart + this.viewLen();
    c.textAlign = 'center'; c.textBaseline = 'middle';
    this.feats.forEach(function (f) {
      var col = typeColor(f.type), y = this.laneY(f.lane), h = LANE_H - 6, edge = G.partialEdge(f);
      this.segments(f).forEach(function (seg) {
        var x0 = Math.max(GUTTER, this.x(seg[0])), x1 = Math.min(this.wrap.clientWidth, this.x(seg[1] + 1));
        if (x1 - x0 < 1) return;
        var fwd = f.dir !== '-', head = Math.min(8, x1 - x0);
        // vertices in drawing order; pts[4]-pts[0] is the closing edge.
        var pts = fwd ? [[x0, y], [x1 - head, y], [x1, y + h / 2], [x1 - head, y + h], [x0, y + h]]
                      : [[x1, y], [x0 + head, y], [x0, y + h / 2], [x0 + head, y + h], [x1, y + h]];
        c.beginPath();
        c.moveTo(pts[0][0], pts[0][1]);
        for (var i = 1; i < pts.length; i++) c.lineTo(pts[i][0], pts[i][1]);
        c.closePath();
        c.fillStyle = col + '55'; c.fill();
        c.lineWidth = f.row === this.selected ? 2 : 1;
        c.strokeStyle = f.row === this.selected ? cssVar('--mp-primary', '#337ab7') : col;

        // the flat closing edge (pts[4]-pts[0]) is the pos1 side when fwd,
        // pos2 side otherwise; the tip (pts[1..3]) is the other side.
        var owns = edge ? G.segOwns(seg, vs, ve) : null;
        var dashStart = !!edge && (edge === 'start' || edge === 'both') && owns.start;
        var dashEnd = !!edge && (edge === 'end' || edge === 'both') && owns.end;
        var dashFlat = fwd ? dashStart : dashEnd;
        var dashTip = fwd ? dashEnd : dashStart;

        c.setLineDash([]);
        c.beginPath(); c.moveTo(pts[0][0], pts[0][1]); c.lineTo(pts[1][0], pts[1][1]); c.stroke();
        c.beginPath(); c.moveTo(pts[3][0], pts[3][1]); c.lineTo(pts[4][0], pts[4][1]); c.stroke();
        c.setLineDash(dashTip ? [3, 2] : []);
        c.beginPath(); c.moveTo(pts[1][0], pts[1][1]); c.lineTo(pts[2][0], pts[2][1]); c.lineTo(pts[3][0], pts[3][1]); c.stroke();
        c.setLineDash(dashFlat ? [3, 2] : []);
        c.beginPath(); c.moveTo(pts[4][0], pts[4][1]); c.lineTo(pts[0][0], pts[0][1]); c.stroke();
        c.setLineDash([]);

        if (x1 - x0 > c.measureText(f.gene).width + 8) { c.fillStyle = cssVar('--mp-text', '#333'); c.fillText(f.gene, (x0 + x1) / 2, y + h / 2); }
        this.hits.push({ x0: x0, x1: x1, y0: y, y1: y + h, f: f, row: f.row });
      }, this);
    }, this);
  };
  // One bar per pixel column, the max of the positions under it.
  Viewer.prototype.drawTrack = function (c, W, y, h, vals, vmax, label, flag) {
    var base = cssVar('--mp-primary', '#337ab7'), red = '#e04b5a', muted = cssVar('--mp-text-muted', '#6a6a6a');
    c.fillStyle = cssVar('--mp-surface-alt', '#f5f5f5'); c.fillRect(GUTTER, y, W - GUTTER, h);
    for (var px = GUTTER; px < W; px++) {
      var a = Math.floor(this.viewStart + (px - GUTTER) / this.ppb), b = Math.floor(this.viewStart + (px + 1 - GUTTER) / this.ppb);
      var v = null;
      for (var lin = a; lin <= b; lin++) {
        var bp = this.baseAt(lin); if (!bp) continue;
        var d = vals[bp.pos - 1]; if (d !== null && d !== undefined && (v === null || d > v)) v = d;
      }
      if (v === null) continue;
      var bh = Math.min(h, Math.round(v / vmax * h));
      c.fillStyle = flag && v > ERR_FLAG ? red : base;
      c.fillRect(px, y + h - bh, 1, bh);
    }
    c.strokeStyle = cssVar('--mp-border', '#ccc'); c.beginPath(); c.moveTo(GUTTER, y + h + 0.5); c.lineTo(W, y + h + 0.5); c.stroke();
    if (flag) { c.save(); c.strokeStyle = red; c.setLineDash([2, 3]); var fy = y + h - ERR_FLAG / vmax * h;
      c.beginPath(); c.moveTo(GUTTER, fy + 0.5); c.lineTo(W, fy + 0.5); c.stroke(); c.restore(); }
    c.fillStyle = muted; c.textAlign = 'right';
    c.textBaseline = 'top'; c.fillText(flag ? (vmax * 100).toFixed(0) + '%' : String(vmax), GUTTER - 6, y);
    c.textBaseline = 'bottom'; c.fillText(label, GUTTER - 6, y + h);
  };
  Viewer.prototype.drawNt = function (c, y) {
    var vs = Math.floor(this.viewStart), ve = Math.ceil(this.viewStart + this.viewLen());
    c.textAlign = 'center'; c.textBaseline = 'middle';
    for (var lin = vs; lin <= ve; lin++) {
      var b = this.baseAt(lin); if (!b) continue;
      var x = this.x(lin), col = BASE[b.base] || BASE.N;
      c.fillStyle = col + (this.ppb >= NT_LETTER ? '99' : 'cc');
      c.fillRect(x, y + 2, Math.max(1, this.ppb - (this.ppb >= NT_LETTER ? 1 : 0)), NT_H - 4);
      if (this.ppb >= NT_LETTER) { c.fillStyle = '#000000'; c.fillText(b.base, x + this.ppb / 2, y + NT_H / 2); }
    }
    c.fillStyle = cssVar('--mp-text-muted', '#6a6a6a'); c.textAlign = 'right';
    c.fillText('nt', GUTTER - 6, y + NT_H / 2);
  };
  Viewer.prototype.drawAa = function (c, f, y) {
    var n = G.nCodons(f, this.len), vs = this.viewStart, ve = vs + this.viewLen();
    c.textAlign = 'right'; c.textBaseline = 'middle'; c.fillStyle = cssVar('--mp-text-muted', '#6a6a6a');
    c.fillText(f.gene, GUTTER - 6, y + AA_H / 2);
    c.textAlign = 'center';
    var ks = this.topology === 'circular' ? [-1, 0, 1] : [0];
    for (var i = 0; i < n; i++) {
      var letter = G.stopLetter(f, i, this.len); if (!letter) continue;
      var centre = G.codonCentre(f, i, this.len, this.topology);
      ks.forEach(function (k) {
        var lin = centre + k * this.len;
        if (lin < vs - 1 || lin > ve + 1) return;
        var x = this.x(lin) + this.ppb / 2;
        var stop = letter === '*', zc = ZAPPO[letter];
        c.fillStyle = stop ? '#000000' : zc || cssVar('--mp-surface-alt', '#f5f5f5');
        c.fillRect(x - 1.5 * this.ppb + 1, y + 2, 3 * this.ppb - 2, AA_H - 4);
        c.fillStyle = stop ? '#ffffff' : zc ? '#222222' : cssVar('--mp-text', '#333');
        c.fillText(letter, x, y + AA_H / 2);
        this.hits.push({ x0: x - 1.5 * this.ppb, x1: x + 1.5 * this.ppb, y0: y, y1: y + AA_H, f: f, row: f.row, codon: i, letter: letter });
      }, this);
    }
  };

  // ---- interaction ----
  Viewer.prototype.hitTest = function (px, py) {
    for (var i = this.hits.length - 1; i >= 0; i--) {
      var h = this.hits[i];
      if (px >= h.x0 && px <= h.x1 && py >= h.y0 && py <= h.y1) return h;
    }
    return null;
  };
  Viewer.prototype.click = function (px, py) {
    var h = this.hitTest(px, py);
    this.selected = h ? h.f.row : null;
    if (h && this.input && window.Shiny) {
      window.Shiny.setInputValue(this.input, { row: h.f.row, nonce: Date.now() }, { priority: 'event' });
    }
    this.draw();
  };
  Viewer.prototype.bindControls = function () {
    var self = this, sec = this.section || document, prefix = this.id.replace(/-canvas$/, '');
    var cv = this.canvas, dragging = null;
    cv.addEventListener('wheel', function (e) {
      e.preventDefault();
      var rect = cv.getBoundingClientRect(), px = e.clientX - rect.left;
      var pan = e.shiftKey ? e.deltaY : Math.abs(e.deltaX) > Math.abs(e.deltaY) ? e.deltaX : 0;
      if (pan) { self.viewStart += pan / self.ppb; self.clamp(); self.draw(); return; }
      self.zoom(Math.pow(1.15, -e.deltaY / 100), self.viewStart + (px - GUTTER) / self.ppb);
    }, { passive: false });
    cv.addEventListener('mousedown', function (e) { dragging = { x: e.clientX, start: self.viewStart, moved: false }; });
    window.addEventListener('mousemove', function (e) {
      if (!dragging) return;
      var dx = e.clientX - dragging.x; if (Math.abs(dx) > 2) dragging.moved = true;
      self.viewStart = dragging.start - dx / self.ppb; self.clamp(); self.draw();
    });
    window.addEventListener('mouseup', function (e) {
      if (!dragging) return;
      var rect = cv.getBoundingClientRect();
      if (!dragging.moved) self.click(e.clientX - rect.left, e.clientY - rect.top);
      dragging = null;
    });
    cv.addEventListener('mousemove', function (e) {
      var rect = cv.getBoundingClientRect(), px = e.clientX - rect.left, py = e.clientY - rect.top;
      var h = self.hitTest(px, py), b = self.baseAt(self.viewStart + (px - GUTTER) / self.ppb);
      if (!self.tip) return;
      if (!b) { self.tip.hidden = true; return; }
      var t = 'Position ' + b.pos.toLocaleString() + ', ' + b.base;
      if (self.depth && self.depth[b.pos - 1] !== null) t += ' | depth ' + self.depth[b.pos - 1];
      if (self.err && self.err[b.pos - 1] !== null) t += ', error ' + (self.err[b.pos - 1] * 100).toFixed(1) + '%';
      if (h) t += ' | ' + h.f.gene + (h.codon !== undefined ? ' codon ' + (h.codon + 1) + ' ' + h.letter : ' (' + h.f.type + ')');
      if (h && typeof h.f.notes === 'string' && h.f.notes) t += ' | ' + h.f.notes;
      self.tip.textContent = t; self.tip.hidden = false;
      var tw = self.tip.offsetWidth, th = self.tip.offsetHeight;
      self.tip.style.left = Math.max(0, Math.min(px + 12, cv.clientWidth - tw)) + 'px';
      self.tip.style.top = (py + 12 + th > cv.clientHeight ? py - th - 8 : py + 12) + 'px';
    });
    cv.addEventListener('mouseleave', function () { if (self.tip) self.tip.hidden = true; });
    sec.querySelectorAll('[data-mpseq]').forEach(function (btn) {
      btn.addEventListener('click', function () {
        var a = btn.getAttribute('data-mpseq');
        if (a === 'zoom_in') self.zoom(2); else if (a === 'zoom_out') self.zoom(0.5);
        else if (a === 'whole') self.whole(); else if (a === 'fit' && self.selected !== null) self.fit(self.selected);
      });
    });
    var go = document.getElementById(prefix + '-goto');
    [['show_nt', 'showNt'], ['show_aa', 'showAa'], ['show_cov', 'showCov'], ['show_err', 'showErr']].forEach(function (pair) {
      var box = document.getElementById(prefix + '-' + pair[0]);
      if (box) box.addEventListener('change', function () { self[pair[1]] = box.checked; self.draw(); });
    });
    if (go) go.addEventListener('keydown', function (e) { if (e.key === 'Enter') { var v = parseInt(go.value, 10); if (v >= 1 && v <= self.len) self.goto(v); } });
  };

  function get(id) {
    var el = document.getElementById(id);
    if (!el) return null;
    if (viewers[id] && viewers[id].canvas !== el) delete viewers[id];
    return viewers[id] || (viewers[id] = new Viewer(id));
  }
  if (window.Shiny) {
    window.Shiny.addCustomMessageHandler('mpseq', function (p) { var v = get(p.id); if (v) v.load(p); });
    window.Shiny.addCustomMessageHandler('mpseq_select', function (p) { var v = get(p.id); if (v) v.fit(p.row); });
  }
  window.mpseq.state = function (id) {
    var v = viewers[id]; if (!v) return null;
    return { len: v.len, version: v.version, topology: v.topology, viewStart: v.viewStart, ppb: v.ppb, selected: v.selected, height: v.height(),
             nLanes: v.nLanes, features: v.feats.map(function (f) { return { row: f.row, gene: f.gene, pos1: f.pos1, pos2: f.pos2, lane: f.lane }; }) };
  };
  window.mpseq.zoom = function (id, f) { var v = get(id); if (v) v.zoom(f); };
  window.mpseq.fit = function (id, row) { var v = get(id); if (v) v.fit(row); };
  window.mpseq.whole = function (id) { var v = get(id); if (v) v.whole(); };
  window.mpseq.goto = function (id, pos) { var v = get(id); if (v) v.goto(pos); };
  window.mpseq.laneY = function (id, lane) { var v = get(id); return v ? v.laneY(lane) + 8 : 0; };
  window.mpseq.hitTest = function (id, px, py) { var v = get(id); return v ? v.hitTest(px, py) : null; };
  window.mpseq.click = function (id, px, py) { var v = get(id); if (v) v.click(px, py); };
})();
