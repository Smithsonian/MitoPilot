// Update horizontal scroll position
$( document ).ready(function(){
  Shiny.addCustomMessageHandler('hScroll', function(params) {
    // console.log(params.id)
    var elmnt = document.getElementById(params.id);
    elmnt.scrollLeft = params.px;
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

// Mousewheel -> horizontal scroll for the annotate-details alignment views.
// The coverage map and synteny PNGs sit in overflow-x:auto containers
// (coverageDiv / syntenyScrollDiv); the MSA viewer scrolls its own
// .biojs_msa_rheader element. Translate vertical wheel delta into horizontal
// scroll (down = right, up = left) and suppress page scroll over these views.
$( document ).ready(function(){
  document.addEventListener('wheel', function(e) {
    if (!e.target.closest) return;
    var delta = e.deltaY;
    if (!delta) return;

    var box = e.target.closest('[id$="coverageDiv"], [id$="syntenyScrollDiv"]');
    if (box) {
      box.scrollLeft += delta;
      e.preventDefault();
      return;
    }

    var msa = e.target.closest('.msaR');
    if (msa) {
      var header = msa.querySelector('.biojs_msa_rheader')
                || document.getElementsByClassName('biojs_msa_rheader')[0];
      if (header) {
        header.scrollLeft += delta;
        header.dispatchEvent(new Event('scroll'));
        e.preventDefault();
      }
    }
  }, { passive: false });
});

// Add an "All" choice to the sample-table page-size dropdowns. reactable
// (0.4.5) has no native "All", so append an option with a very large page size
// (shows every filtered row). A MutationObserver re-adds it after reactable
// re-renders the select.
$( document ).ready(function(){
  var GATED_ALL = '#assemble-table, #annotate-table, #export-table';
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
  // Restrict to the Assemble / Annotate / Export sample tables (output id
  // "table" namespaced by module: assemble_server("assemble"), etc.).
  var GATED = '#assemble-table, #annotate-table, #export-table';
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
