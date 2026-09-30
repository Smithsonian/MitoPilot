// Metadata modal: Show and Export ticks kept per table here, drawn into
// reactable cells by mpMV.box, sent to Shiny on Save.
window.mpMV = {
  st: {},
  init: function(o) {
    this.st[o.tbl] = {
      show: new Set([].concat(o.show || [])), export: new Set([].concat(o.export || [])),
      exportable: new Set([].concat(o.exportable || [])), link: !!o.link, source: o.source
    };
    this.whenReady(o.tbl, function() {
      if (o.source) {
        $(document.getElementById(o.tbl)).closest('.modal')
          .find('.mp-mv-toggle[data-src="' + o.source + '"]').addClass('active');
        Reactable.setFilter(o.tbl, 'source', [o.source]);
      }
      Reactable.onStateChange(o.tbl, function() { mpMV.sync(o.tbl); });
      mpMV.sync(o.tbl);
    });
  },
  whenReady: function(tbl, fn, tries) {
    tries = tries || 0;
    var inst = null;
    try { inst = Reactable.getInstance(tbl); } catch (e) {}
    if (inst) return fn();
    if (tries < 100) setTimeout(function() { mpMV.whenReady(tbl, fn, tries + 1); }, 50);
  },
  box: function(tbl, key, kind) {
    var s = this.st[tbl];
    if (!s) return '';
    if (kind === 'export' && !s.exportable.has(key)) {
      return '<span class="mp-mv-na" title="Map file columns are always available at export">always</span>';
    }
    var k = String(key).replace(/"/g, '&quot;');
    return '<input type="checkbox" class="mp-mv-box" data-tbl="' + tbl + '" data-kind="' + kind +
      '" data-key="' + k + '"' + (s[kind].has(key) ? ' checked' : '') + ' aria-label="' + kind + '">';
  },
  ticked: function(tbl, key) {
    var s = this.st[tbl];
    return s && (s.show.has(key) || s.export.has(key));
  },
  set: function(tbl, key, kind, on) {
    var s = this.st[tbl];
    var kinds = s.link ? ['show', 'export'] : [kind];
    kinds.forEach(function(k) {
      if (k === 'export' && !s.exportable.has(key)) return;
      if (on) s[k].add(key); else s[k].delete(key);
    });
  },
  visible: function(tbl) {
    return Reactable.getState(tbl).sortedData.map(function(r) { return r.key; });
  },
  sync: function(tbl) {
    var s = this.st[tbl];
    var root = document.getElementById(tbl);
    if (!s || !root) return;
    root.querySelectorAll('input.mp-mv-box').forEach(function(el) {
      el.checked = s[el.dataset.kind].has(el.dataset.key);
    });
    var keys = [];
    try { keys = this.visible(tbl); } catch (e) {}
    root.querySelectorAll('input.mp-mv-all').forEach(function(el) {
      var k = el.dataset.kind;
      var elig = keys.filter(function(x) { return k === 'show' || s.exportable.has(x); });
      var n = elig.filter(function(x) { return s[k].has(x); }).length;
      el.checked = elig.length > 0 && n === elig.length;
      el.indeterminate = n > 0 && n < elig.length;
      el.disabled = !elig.length;
    });
    var c = document.getElementById(tbl + '_count');
    if (c) c.textContent = s.show.size + ' shown, ' + s.export.size + ' export';
  },
  toggleSource: function(tbl, btn) {
    btn.classList.toggle('active');
    var on = $(btn).closest('.modal').find('.mp-mv-toggle.active[data-src]')
      .map(function() { return this.dataset.src; }).get();
    Reactable.setFilter(tbl, 'source', on.length ? on : undefined);
  },
  clear: function(tbl) {
    ['source', 'level', 'show'].forEach(function(c) { Reactable.setFilter(tbl, c, undefined); });
    $(document.getElementById(tbl)).closest('.modal').find('.mp-mv-toggle').removeClass('active');
  },
  toggleFilter: function(tbl, col, value, btn) {
    var on = !btn.classList.contains('active');
    btn.classList.toggle('active', on);
    Reactable.setFilter(tbl, col, on ? value : undefined);
  },
  copy: function(el) {
    var t = el.textContent;
    var done = function() {
      el.classList.add('mp-mv-copied');
      setTimeout(function() { el.classList.remove('mp-mv-copied'); }, 900);
    };
    var legacy = function() {
      var ta = document.createElement('textarea');
      ta.value = t;
      el.parentNode.appendChild(ta);
      ta.select();
      try { if (document.execCommand('copy')) done(); } catch (e) {}
      ta.remove();
    };
    if (navigator.clipboard) navigator.clipboard.writeText(t).then(done, legacy); else legacy();
  },
  save: function(tbl, inputId) {
    var s = this.st[tbl];
    Shiny.setInputValue(inputId, { show: Array.from(s.show), export: Array.from(s.export), link: s.link },
                        { priority: 'event' });
  }
};

$(document).on('change', 'input.mp-mv-box', function() {
  mpMV.set(this.dataset.tbl, this.dataset.key, this.dataset.kind, this.checked);
  mpMV.sync(this.dataset.tbl);
});
$(document).on('change', 'input.mp-mv-all', function() {
  var tbl = this.dataset.tbl, kind = this.dataset.kind, on = this.checked;
  mpMV.visible(tbl).forEach(function(k) { mpMV.set(tbl, k, kind, on); });
  mpMV.sync(tbl);
});
// A header checkbox must not also sort or resize the column
$(document).on('click mousedown', 'label.mp-mv-head', function(e) { e.stopPropagation(); });
