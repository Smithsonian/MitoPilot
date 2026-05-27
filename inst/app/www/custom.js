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

// auto scrolling of progress box
$( document ).ready(function(){
  Shiny.addCustomMessageHandler("scrollProgress", function(params) {
    var objDiv = document.getElementById(params.id);
    if(objDiv) {
        objDiv.scrollTop = objDiv.scrollHeight
    }
  });
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
