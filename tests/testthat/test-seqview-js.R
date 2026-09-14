sv_page <- function() {
  skip_if_not_installed("chromote")
  skip_if(!nzchar(Sys.getenv("CHROMOTE_CHROME")) && is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
          "no Chrome for chromote")
  b <- chromote::ChromoteSession$new()
  withr::defer(b$close(), envir = parent.frame())
  page <- normalizePath(testthat::test_path("seqview", "index.html"))
  b$Page$navigate(paste0("file://", page))
  Sys.sleep(1)
  b
}
js <- function(b, code) b$Runtime$evaluate(code, returnByValue = TRUE)$result$value

test_that("lanes pack overlapping features and keep a wrapped feature in one lane", {
  b <- sv_page()
  n <- js(b, "(function(){var g=window.mpseq.geom; var f=[
    {pos1:10,pos2:100,dir:'+'},{pos1:90,pos2:200,dir:'+'},{pos1:150,pos2:160,dir:'-'},
    {pos1:16500,pos2:120,dir:'+'}]; var len=16600; var n=g.lanes(f,len);
    return JSON.stringify({n:n, lanes:f.map(function(x){return x.lane;})});})()")
  expect_equal(jsonlite::fromJSON(n), list(n = 3L, lanes = c(0L, 1L, 0L, 2L)))
})

test_that("codon centres follow the strand and wrap on a circular unit", {
  b <- sv_page()
  r <- js(b, "(function(){var g=window.mpseq.geom; var len=100;
    var plus={pos1:10,pos2:18,dir:'+'}, minus={pos1:10,pos2:18,dir:'-'}, wrap={pos1:98,pos2:6,dir:'+'};
    return JSON.stringify([g.codonCentre(plus,0,len,'linear'), g.codonCentre(plus,2,len,'linear'),
      g.codonCentre(minus,0,len,'linear'), g.codonCentre(minus,2,len,'linear'),
      g.codonCentre(wrap,0,len,'circular'), g.codonCentre(wrap,1,len,'circular'),
      g.nCodons(wrap,len), g.span(wrap,len)]);})()")
  expect_equal(jsonlite::fromJSON(r), c(11, 17, 17, 11, 99, 2, 3, 9))
})

test_that("the stop letter is drawn only for a stop codon beyond the translation", {
  b <- sv_page()
  r <- js(b, "(function(){var g=window.mpseq.geom; var len=1000;
    var withStop={pos1:1,pos2:9,dir:'+',translation:'MK'}, trimmed={pos1:1,pos2:9,dir:'+',translation:'MKL'};
    return JSON.stringify([g.stopLetter(withStop,2,len), g.stopLetter(withStop,1,len), g.stopLetter(trimmed,2,len)]);})()")
  expect_equal(jsonlite::fromJSON(r), c("*", "K", "L"))
})

test_that("the viewer loads a payload, fits a gene, and reports clicks", {
  b <- sv_page()
  r <- js(b, "(function(){
    var seq = Array(2000).join('ACGT').slice(0, 2000);
    window.__handlers.mpseq({id:'sv-canvas', input:'sv-pick', unit:'S1.1.1', len:2000, topology:'circular', seq:seq, version:1,
      selected:null, features:[{row:1,type:'PCG',gene:'nad1',pos1:100,pos2:399,dir:'+',partial5:false,partial3:false,notes:'',translation:'MKL'},
                               {row:3,type:'tRNA',gene:'trnF',pos1:380,pos2:450,dir:'-',partial5:false,partial3:false,notes:''}]});
    var s0 = window.mpseq.state('sv-canvas');
    window.__handlers.mpseq_select({id:'sv-canvas', row:1});
    var s1 = window.mpseq.state('sv-canvas');
    window.mpseq.whole('sv-canvas'); var s2 = window.mpseq.state('sv-canvas');
    window.mpseq.zoom('sv-canvas', 2); var s3 = window.mpseq.state('sv-canvas');
    window.mpseq.goto('sv-canvas', 1990); var s4 = window.mpseq.state('sv-canvas');
    return JSON.stringify({n:s0.features.length, lanes:s0.nLanes, sel:s1.selected, fitStart:s1.viewStart, fitPpb:s1.ppb,
      wholePpb:s2.ppb, zoomPpb:s3.ppb, gotoStart:s4.viewStart, inputs:window.__inputs.length});})()")
  s <- jsonlite::fromJSON(r)
  expect_equal(s$n, 2); expect_equal(s$lanes, 2); expect_equal(s$sel, 1)
  expect_lt(s$fitStart, 100); expect_gt(s$fitPpb, 1)
  expect_equal(round(s$wholePpb * 2000), 940)   # canvas wrapper is 1000 px wide, minus the 60 px gutter
  expect_equal(round(s$zoomPpb / s$wholePpb), 2)
  # centred on 1990 the view starts before the origin on a circular unit
  expect_gt(s$gotoStart, 1000)
})

test_that("a click on a gene arrow sends the row through the Shiny input", {
  b <- sv_page()
  r <- js(b, "(function(){
    window.__inputs = [];
    var seq = Array(2000).join('ACGT').slice(0, 2000);
    window.__handlers.mpseq({id:'sv-canvas', input:'sv-pick', unit:'S1.1.1', len:2000, topology:'linear', seq:seq, version:1,
      selected:null, features:[{row:2,type:'PCG',gene:'cox1',pos1:1,pos2:2000,dir:'+',partial5:false,partial3:false,notes:'',translation:'M'}]});
    window.mpseq.whole('sv-canvas');
    var hit = window.mpseq.hitTest('sv-canvas', 500, window.mpseq.laneY('sv-canvas', 0));
    window.mpseq.click('sv-canvas', 500, window.mpseq.laneY('sv-canvas', 0));
    return JSON.stringify({hit: hit && hit.row, sent: window.__inputs.map(function(i){return i.name+':'+i.value.row;})});})()")
  s <- jsonlite::fromJSON(r)
  expect_equal(s$hit, 2)
  expect_equal(s$sent, "sv-pick:2")
})

test_that("partialEdge picks the genomic side and segOwns finds which piece is real", {
  b <- sv_page()
  r <- js(b, "(function(){
    var g = window.mpseq.geom;
    var edges = [
      g.partialEdge({dir:'+',partial5:true,partial3:false}),
      g.partialEdge({dir:'+',partial5:false,partial3:true}),
      g.partialEdge({dir:'-',partial5:true,partial3:false}),
      g.partialEdge({dir:'-',partial5:false,partial3:true})
    ];
    var len = 16600, f = {pos1:16500, pos2:120, dir:'+'};
    var span = g.span(f, len);
    var segA = [f.pos1, f.pos1 + span - 1, 0];               // the piece that starts at pos1
    var segB = [f.pos1 - len, f.pos1 - len + span - 1, -1];  // the piece that ends at pos2
    var ownsA = g.segOwns(segA, 1, len + 1);
    var ownsB = g.segOwns(segB, 1, len + 1);
    return JSON.stringify({edges: edges, ownsA: ownsA, ownsB: ownsB});
  })()")
  s <- jsonlite::fromJSON(r)
  expect_equal(s$edges, c("start", "end", "end", "start"))
  expect_true(s$ownsA$start); expect_false(s$ownsA$end)
  expect_false(s$ownsB$start); expect_true(s$ownsB$end)
})

test_that("swapping the canvas node drops the stale viewer instead of drawing into nothing", {
  b <- sv_page()
  r <- js(b, "(function(){
    var seq = Array(2000).join('ACGT').slice(0, 2000);
    var small = {id:'sv-canvas', input:'sv-pick', unit:'S1.1.1', len:2000, topology:'linear', seq:seq, version:1,
      selected:null, features:[{row:5,type:'PCG',gene:'nad2',pos1:1,pos2:200,dir:'+',partial5:false,partial3:false,notes:'',translation:'M'}]};
    window.__handlers.mpseq(small);
    window.mpseq.whole('sv-canvas');
    var missBefore = window.mpseq.hitTest('sv-canvas', 500, window.mpseq.laneY('sv-canvas', 0));
    var wrap = document.getElementById('sv-canvas').parentElement;
    wrap.removeChild(document.getElementById('sv-canvas'));
    var fresh = document.createElement('canvas'); fresh.id = 'sv-canvas';
    wrap.appendChild(fresh);
    // a new version with the gene spanning the whole genome: only a redraw against
    // the fresh, attached canvas will produce a hit at px 500
    var full = {id:'sv-canvas', input:'sv-pick', unit:'S1.1.1', len:2000, topology:'linear', seq:seq, version:2,
      selected:null, features:[{row:5,type:'PCG',gene:'nad2',pos1:1,pos2:2000,dir:'+',partial5:false,partial3:false,notes:'',translation:'M'}]};
    window.__handlers.mpseq(full);
    var hit = window.mpseq.hitTest('sv-canvas', 500, window.mpseq.laneY('sv-canvas', 0));
    return JSON.stringify({missBefore: missBefore, hit: hit && hit.row, isFresh: document.getElementById('sv-canvas') === fresh});
  })()")
  s <- jsonlite::fromJSON(r)
  expect_null(s$missBefore)
  expect_equal(s$hit, 5)
  expect_true(s$isFresh)
})

test_that("the scale self-corrects once the container's real width is known", {
  b <- sv_page()
  r <- js(b, "(function(){
    var wrap = document.getElementById('sv-canvas').parentElement;
    wrap.style.display = 'none';
    var seq = Array(2000).join('ACGT').slice(0, 2000);
    window.__handlers.mpseq({id:'sv-canvas', input:'sv-pick', unit:'S1.1.1', len:2000, topology:'linear', seq:seq, version:1,
      selected:null, features:[]});
    wrap.style.display = '';
    document.getElementById('sv-section').dispatchEvent(new Event('toggle'));
    var s = window.mpseq.state('sv-canvas');
    return JSON.stringify({ppb: s.ppb});
  })()")
  s <- jsonlite::fromJSON(r)
  expect_equal(round(s$ppb * 2000), 940)
})

test_that("a payload selection outlines a row and a null selection clears it", {
  b <- sv_page()
  r <- js(b, "(function(){
    var seq = Array(2000).join('ACGT').slice(0, 2000);
    var p = {id:'sv-canvas', input:'sv-pick', unit:'S1.1.1', len:2000, topology:'linear', seq:seq, version:1,
      selected:2, features:[{row:2,type:'PCG',gene:'cox1',pos1:1,pos2:900,dir:'+',partial5:false,partial3:false,notes:'',translation:'M'}]};
    window.__handlers.mpseq(p);
    var first = window.mpseq.state('sv-canvas').selected;
    p.selected = null;
    window.__handlers.mpseq(p);
    var cleared = window.mpseq.state('sv-canvas').selected;
    return JSON.stringify({first: first, cleared: cleared === null});})()")
  s <- jsonlite::fromJSON(r)
  expect_equal(s$first, 2)
  expect_true(s$cleared)
})

test_that("codon centres of a minus-strand wrapped feature step back through the origin", {
  b <- sv_page()
  r <- js(b, "(function(){var g=window.mpseq.geom, len=100, f={pos1:95,pos2:6,dir:'-'};
    return JSON.stringify([g.span(f,len), g.nCodons(f,len),
      g.codonCentre(f,0,len,'circular'), g.codonCentre(f,1,len,'circular'),
      g.codonCentre(f,2,len,'circular'), g.codonCentre(f,3,len,'circular')]);})()")
  expect_equal(jsonlite::fromJSON(r), c(12, 4, 5, 2, 99, 96))
})

test_that("joined pieces draw their connector and stay in the state", {
  b <- sv_page()
  r <- js(b, "(function(){
    var seq = Array(2000).join('ACGT').slice(0, 2000), j = 'JOIN: mode=exon group=1';
    window.__handlers.mpseq({id:'sv-canvas', input:'sv-pick', unit:'S1.1.1', len:2000, topology:'linear', seq:seq, version:7,
      selected:null, features:[
        {row:1,type:'PCG',gene:'cox1',pos1:100,pos2:399,dir:'+',partial5:false,partial3:false,notes:j,joined:j,translation:'MKL'},
        {row:2,type:'PCG',gene:'cox1',pos1:800,pos2:1099,dir:'+',partial5:false,partial3:false,notes:j,joined:j,translation:'MKL'}]});
    window.mpseq.whole('sv-canvas');
    var s = window.mpseq.state('sv-canvas');
    return JSON.stringify({rows: s.features.map(function(f){return f.row;}), lanes: s.nLanes});})()")
  s <- jsonlite::fromJSON(r)
  expect_equal(s$rows, c(1, 2))
  expect_equal(s$lanes, 1)
})

test_that("coverage and error tracks add height above the lanes and toggle off", {
  b <- sv_page()
  r <- js(b, "(function(){
    var seq = Array(2000).join('ACGT').slice(0, 2000), depth = [], err = [];
    for (var i = 0; i < 2000; i++) { depth.push(50 + (i % 7)); err.push(i === 100 ? 0.2 : 0.001); }
    var feats = [{row:1,type:'tRNA',gene:'trnF',pos1:10,pos2:80,dir:'+',partial5:false,partial3:false,notes:''}];
    var base = {id:'sv-canvas', input:'sv-pick', unit:'S1.1.1', len:2000, topology:'linear', seq:seq, version:1, selected:null, features:feats};
    window.__handlers.mpseq(base); var h0 = window.mpseq.state('sv-canvas').height, lane0 = window.mpseq.laneY('sv-canvas', 0);
    window.__handlers.mpseq(Object.assign({depth:depth, err:err}, base));
    var h1 = window.mpseq.state('sv-canvas').height, lane1 = window.mpseq.laneY('sv-canvas', 0);
    var box = document.getElementById('sv-show_cov'); box.checked = false; box.dispatchEvent(new Event('change'));
    var h2 = window.mpseq.state('sv-canvas').height;
    box = document.getElementById('sv-show_err'); box.checked = false; box.dispatchEvent(new Event('change'));
    var h3 = window.mpseq.state('sv-canvas').height;
    return JSON.stringify({h0:h0, h1:h1, h2:h2, h3:h3, lane0:lane0, lane1:lane1});})()")
  s <- jsonlite::fromJSON(r)
  expect_equal(s$h1 - s$h0, 60 + 12 + 36 + 12)
  expect_equal(s$lane1 - s$lane0, 60 + 12 + 36 + 12)
  expect_equal(s$h2, s$h0 + 36 + 12)
  expect_equal(s$h3, s$h0)
})
