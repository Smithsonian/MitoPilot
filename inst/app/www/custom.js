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


$(document).ready(function() {
  Shiny.addCustomMessageHandler('rightScroll', function(params) {
    function waitForElement(selector, callback) {
      const checkExist = setInterval(() => {
        const elmnt = document.querySelector(selector);
        if (elmnt) {
          clearInterval(checkExist);
          callback(elmnt);
        }
      }, 50); // Check every 50ms
    }

    waitForElement('.biojs_msa_rheader', function(elmnt) {
      elmnt.scrollLeft = elmnt.scrollWidth - elmnt.clientWidth;
    });
  });
});
