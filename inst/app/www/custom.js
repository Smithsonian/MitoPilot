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

