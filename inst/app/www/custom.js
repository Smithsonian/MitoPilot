// The Assemble / Annotate / Export sample tables, named once. The output id
// is "table" namespaced by module, so the three ids below are what the app
// mounts today; .mp-sample-table is the class the modules are moving to.
window.MP_SAMPLE_TABLES =
  '#assemble-table, #annotate-table, #export-table, .mp-sample-table';

// One help affordance: every [data-toggle="mp-popover"] on the page (modals
// and dynamically rendered UI included) opens its data-content in a
// Bootstrap 3 popover on click. Opened and closed by hand, one at a time: a
// click elsewhere, Escape, scrolling, a modal opening or closing, or the icon
// being re-rendered away closes it, so none is left stranded on screen.
$( document ).ready(function(){
  if (!$.fn.popover) return;
  var open = null;
  window.mpHidePopovers = function() {
    if (open) {
      $(open).attr('aria-expanded', 'false').popover('destroy');
      open = null;
    }
    $('body > .popover').remove();
  };
  $(document).on('click', '[data-toggle="mp-popover"]', function(e) {
    e.preventDefault();
    e.stopPropagation();
    var el = this, again = open === el;
    mpHidePopovers();
    if (again) return;
    $(el).popover({ trigger: 'manual', placement: 'auto right', container: 'body', html: true })
      .popover('show').attr('aria-expanded', 'true');
    open = el;
  });
  $(document).on('click', function(e) {
    if (open && !$(e.target).closest('.popover, [data-toggle="mp-popover"]').length) mpHidePopovers();
  });
  $(document).on('keydown', function(e) {
    if (e.key === 'Escape' && open) mpHidePopovers();
  });
  document.addEventListener('scroll', function(e) {
    var t = e.target;
    if (open && !(t.closest && t.closest('.popover'))) mpHidePopovers();
  }, true);
  $(document).on('show.bs.modal hide.bs.modal', function() { mpHidePopovers(); });
  $(document).on('shiny:value', function() {
    setTimeout(function() { if (open && !document.body.contains(open)) mpHidePopovers(); }, 50);
  });
});

// Update horizontal scroll position
$( document ).ready(function(){
  Shiny.addCustomMessageHandler('hScroll', function(params) {
    // console.log(params.id)
    var elmnt = document.getElementById(params.id);
    if (elmnt) elmnt.scrollLeft = params.px;
  });
});

// Update open/close state of details elements
$( document ).ready(function(){
  Shiny.addCustomMessageHandler('toggleDetails', function(params) {
    // console.log(params.id);
    $("#" + params.id).attr('open', params.state);
  });
});

// Clipboard helper
$( document ).ready(function(){
  Shiny.addCustomMessageHandler('copy_to_clipboard', function(params) {
    navigator.clipboard.writeText(params.text)
  });
});


$( document ).ready(function(){
  Shiny.addCustomMessageHandler('rightScroll', function(params) {
    // Get the header element
    var header = document.getElementsByClassName('biojs_msa_rheader')[0];
    var maxScrollLeft = header.scrollWidth - header.clientWidth;
    header.scrollLeft = maxScrollLeft;
    var scrollEvent = new Event('scroll');
    header.dispatchEvent(scrollEvent);
  });
});

// Center the MSA viewer on a given alignment column (conflict-block navigation)
// Scrolls the same horizontally-overflowing element the rightScroll handler
// uses (.biojs_msa_rheader); the seqblock is a fixed-size canvas and does not
// itself overflow, so scrolling it has no effect.
$( document ).ready(function(){
  Shiny.addCustomMessageHandler("msaScrollToCol", function(params) {
    var attempts = 0;
    function tryScroll() {
      var el = document.getElementsByClassName('biojs_msa_rheader')[0]
            || document.getElementsByClassName('biojs_msa_seqblock')[0];
      if (!el || !el.scrollWidth || el.scrollWidth <= el.clientWidth) {
        if (attempts++ < 20) { setTimeout(tryScroll, 150); }
        return;
      }
      var colWidth = el.scrollWidth / params.alnLen;
      var target = (params.col - 0.5) * colWidth - el.clientWidth / 2;
      var maxScroll = el.scrollWidth - el.clientWidth;
      target = Math.max(0, Math.min(target, maxScroll));
      el.scrollLeft = target;
      el.dispatchEvent(new Event('scroll'));
    }
    tryScroll();
  });
});

// auto scrolling of progress box
$( document ).ready(function(){
  Shiny.addCustomMessageHandler("scrollProgress", function(params) {
    var objDiv = document.getElementById(params.id);
    if(objDiv) {
        objDiv.scrollTop = objDiv.scrollHeight
    }
  });
});

// Sample-table resize: the CSS handle (resize: vertical) writes an inline
// height, which then sticks and stops the table re-fitting the window. On
// window resize, clear that inline height so the stylesheet calc() takes
// over again and the table stays fully displayed.
$( document ).ready(function(){
  window.addEventListener('resize', function() {
    document.querySelectorAll('.mp-table-resize').forEach(function(el) {
      el.style.height = '';
    });
  });
});

// The annotate-details views (coverage map, synteny, synteny zoom, alignment)
// use native scrolling only: a vertical wheel scrolls the page, and horizontal
// input scrolls the view. Vertical wheel is deliberately NOT translated into
// horizontal scroll here.

// Add an "All" choice to the sample-table page-size dropdowns. reactable
// (0.4.5) has no native "All", so append an option with a very large page size
// (shows every filtered row). A MutationObserver re-adds it after reactable
// re-renders the select.
$( document ).ready(function(){
  var GATED_ALL = window.MP_SAMPLE_TABLES;
  var ALL_PAGE_SIZE = 1000000;

  function addAllOption(select) {
    if (!select || select.querySelector('option[data-mp-all]')) return;
    var opt = document.createElement('option');
    opt.value = String(ALL_PAGE_SIZE);
    opt.text = 'All';
    opt.setAttribute('data-mp-all', '1');
    select.appendChild(opt);
  }

  function refreshAllOptions() {
    document.querySelectorAll(GATED_ALL).forEach(function(tbl) {
      tbl.querySelectorAll('.rt-page-size-select').forEach(addAllOption);
    });
  }

  refreshAllOptions();
  new MutationObserver(refreshAllOptions)
    .observe(document.body, { childList: true, subtree: true });
});

// Shift-click range selection for the main sample reactable tables.
// reactable (0.4.5) has no native range selection, so we drive it on the
// client: a plain click stores an "anchor" row, and a shift-click selects
// every row between the anchor and the clicked row by programmatically
// clicking their selection checkboxes. Additive only (never deselects).
$( document ).ready(function(){
  var GATED = window.MP_SAMPLE_TABLES;
  var anchors = {}; // per-table anchor index, keyed by table element id

  // Rows on the current page that carry a selection checkbox, in visual
  // (DOM) order. Excludes the select-all header (in .rt-thead) and any
  // pagination padding rows (no checkbox).
  function selectableRows(tbody) {
    return Array.prototype.filter.call(
      tbody.querySelectorAll('.rt-tr'),
      function(r) { return r.querySelector('input.rt-select-input'); }
    );
  }

  // Stop shift-click from highlighting cell text inside the gated tables.
  document.addEventListener('mousedown', function(e) {
    if (e.shiftKey && e.target.closest && e.target.closest(GATED)) {
      e.preventDefault();
    }
  }, true);

  document.addEventListener('click', function(e) {
    if (!e.isTrusted) return;                 // ignore our own synthetic clicks
    if (!e.target.closest) return;
    if (e.target.closest('a, button')) return; // leave link/icon actions alone
    var table = e.target.closest(GATED);
    if (!table) return;
    var row = e.target.closest('.rt-tr');
    if (!row || !row.querySelector('input.rt-select-input')) return;
    var tbody = row.closest('.rt-tbody');
    if (!tbody) return;

    var rows = selectableRows(tbody);
    var idx = rows.indexOf(row);
    if (idx < 0) return;
    var id = table.id;
    var anchor = anchors[id];

    if (e.shiftKey && anchor != null && anchor < rows.length) {
      var lo = Math.min(anchor, idx);
      var hi = Math.max(anchor, idx);
      for (var i = lo; i <= hi; i++) {
        if (i === idx) continue;              // reactable toggles the clicked row
        var cb = rows[i].querySelector('input.rt-select-input');
        if (cb && !cb.checked) cb.click();    // additive: only add unchecked rows
      }
      if (window.getSelection) window.getSelection().removeAllRanges();
      // Keep the anchor so successive shift-clicks re-extend from it.
    } else {
      anchors[id] = idx;                      // plain click sets a new anchor
    }
  }, true);
});

// Name the current module in the browser tab, so a window switcher, a
// bookmark and a second window all say which step the tab is on.
$(document).on('shiny:inputchanged', function(e) {
  if (e.name === 'mode' && e.value) document.title = 'MitoPilot - ' + e.value;
});
$(document).on('shiny:connected', function() {
  document.title = 'MitoPilot - ' + ($('#mode input:checked').val() || 'Assemble');
});

// Export Data token chips: a chip inserts its text at the cursor of the header
// box last focused in the same modal (the FASTA header box by default).
$(document).on('focusin', 'textarea', function() {
  var list = $(this).closest('.modal').find('.mp-token-list');
  if (list.length) list.attr('data-target', this.id);
});
$(document).on('click', '.mp-token-chip', function(e) {
  e.preventDefault();
  var box = document.getElementById($(this).closest('.mp-token-list').attr('data-target'));
  if (!box) return;
  var ins = this.getAttribute('data-insert');
  var s = box.selectionStart, t = box.selectionEnd, v = box.value;
  box.value = v.slice(0, s) + ins + v.slice(t);
  box.selectionStart = box.selectionEnd = s + ins.length;
  box.focus();
  $(box).trigger('input').trigger('change');
});
// Chips whose column is empty for some records of the chosen export group turn
// warning-orange, with the count in the tooltip.
function mpTokenFlags(modal) {
  var list = modal.find('.mp-token-list');
  if (!list.length) return;
  var group = modal.find('select[id$="export_group"]').val();
  var totals = JSON.parse(list.attr('data-totals') || '{}');
  var n = totals[group] || 0;
  list.find('.mp-token-chip').each(function() {
    var m = JSON.parse(this.getAttribute('data-missing') || '{}')[group] || 0;
    var base = this.getAttribute('data-title') || '';
    $(this).toggleClass('mp-token-missing', m > 0);
    this.title = m > 0 ? base + '\nMissing for ' + m + ' of ' + n + ' records in this group' : base;
  });
  modal.find('textarea.mp-hl').each(function() { mpHlRender(this); });
}
$(document).on('change', 'select[id$="export_group"]', function() {
  mpTokenFlags($(this).closest('.modal'));
});
$(document).on('shown.bs.modal', function(e) { mpTokenFlags($(e.target)); });
$(document).on('input', '.mp-token-filter', function() {
  var q = this.value.toLowerCase();
  var list = $(this).closest('.mp-token-list');
  list.find('.mp-token-chip').each(function() {
    $(this).toggle($(this).text().toLowerCase().indexOf(q) !== -1);
  });
  if (q) list.find('details').attr('open', '');
});

// Header boxes: a backdrop behind each transparent textarea draws a coloured
// pill under every {field}, keyed by the Available metadata group it is in.
function mpTokMap(modal) {
  var m = {};
  modal.find('.mp-token-group').each(function() {
    var src = (this.className.match(/mp-src-(\w+)/) || [])[1] || 'other';
    $(this).find('.mp-token-chip').each(function() {
      var miss = this.classList.contains('mp-token-missing');
      (this.getAttribute('data-insert').match(/\{[^{}]*\}/g) || []).forEach(function(t) {
        if (!(t in m)) m[t] = {src: src, miss: miss};
      });
    });
  });
  return m;
}
function mpTokSrc(map, t) {
  var src = /^\{nmnh_/.test(t) ? 'nmnh' : t === '{completeness}' ? 'completeness' : (map[t] ? map[t].src : 'other');
  return 'mp-src-' + src + (map[t] && map[t].miss ? ' mp-hl-miss' : '');
}
function mpHlRender(box) {
  var back = box.previousElementSibling;
  if (!back || !back.classList.contains('mp-hl-back')) return;
  var cs = getComputedStyle(box);
  ['fontFamily', 'fontSize', 'lineHeight', 'letterSpacing', 'paddingTop', 'paddingBottom',
   'paddingLeft', 'borderTopWidth', 'borderBottomWidth', 'borderLeftWidth', 'borderRightWidth'
  ].forEach(function(k) { back.style[k] = cs[k]; });
  var sb = Math.max(0, box.offsetWidth - box.clientWidth - parseFloat(cs.borderLeftWidth) - parseFloat(cs.borderRightWidth));
  back.style.paddingRight = (parseFloat(cs.paddingRight) + sb) + 'px';
  var map = mpTokMap($(box).closest('.modal'));
  var esc = box.value.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  back.innerHTML = esc
    .replace(/\[[^\[\]{}=]*=/g, '<span class="mp-hl-mod">$&</span>')
    .replace(/\{[^{}]*\}/g, function(t) { return '<mark class="' + mpTokSrc(map, t) + '">' + t + '</mark>'; }) + '\n';
  back.scrollTop = box.scrollTop;
}
function mpHlInit(modal) {
  modal.find('textarea.mp-hl').each(function() {
    if (!this.previousElementSibling || !this.previousElementSibling.classList.contains('mp-hl-back')) {
      $(this).wrap('<div class="mp-hl-wrap"></div>').before('<div class="mp-hl-back" aria-hidden="true"></div>');
    }
    mpHlRender(this);
  });
  var open = true;
  try { open = localStorage.getItem('mpHdrPreview') !== '0'; } catch (e) {}
  modal.find('details.mp-hdr-preview').prop('open', open);
  // narrow windows: the metadata drawer starts folded
  if (window.matchMedia('(max-width: 819px)').matches) {
    modal.find('.mp-export-side details.mp-meta-panel').prop('open', false);
  }
}
$(document).on('shown.bs.modal', function(e) { mpHlInit($(e.target)); });
$(document).on('input change focus mouseup', 'textarea.mp-hl', function() { mpHlRender(this); });
document.addEventListener('scroll', function(e) {
  if (e.target.classList && e.target.classList.contains('mp-hl')) mpHlRender(e.target);
}, true);
document.addEventListener('toggle', function(e) {
  if (!e.target.classList || !e.target.classList.contains('mp-hdr-preview')) return;
  try { localStorage.setItem('mpHdrPreview', e.target.open ? '1' : '0'); } catch (err) {}
}, true);
$(document).on('shiny:value', function(e) {
  if (!/_preview$/.test(e.name)) return;
  setTimeout(function() {
    var el = $(document.getElementById(e.name));
    var map = mpTokMap(el.closest('.modal'));
    el.find('.mp-hl-tok[data-tok]').each(function() {
      this.className += ' ' + mpTokSrc(map, this.getAttribute('data-tok'));
    });
  }, 0);
});

// A "?" inside a <summary> opens its popover without folding the section
$(document).on('click', 'summary .mp-help-icon', function(e) { e.preventDefault(); });
