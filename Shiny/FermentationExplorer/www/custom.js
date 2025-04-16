// Jump to tab selected with buttons on home page
shinyjs.goToTab = function(tabName) {
  $('a[data-value="' + tabName + '"]').click();
  setTimeout(function() {
    $('a[data-value="' + tabName + '"]').closest('.dropdown-menu').removeClass('show');
  }, 0);
};

// Dynamically resize width based on height
shinyjs.resizeWidthFromHeight = function(containerId, heightToWidthRatio) {
  function resizeContainer() {
    var height = $('#' + containerId).height();
    var newWidth = height * heightToWidthRatio;
    $('#' + containerId).css('width', newWidth + 'px');
  }

  resizeContainer();  // Initial
  $(window).resize(resizeContainer);  // On window resize
};

// Define handlers for checkboxes in History module
function registerJobTableHandlers(jobTableId) {
  Shiny.addCustomMessageHandler(jobTableId + '_get_selected', function(message) {
    var selected = [];
    document.querySelectorAll('input[name="row_selected"]:checked').forEach(function(el) {
      selected.push(el.value);
    });
    Shiny.setInputValue(jobTableId + '_selected_jobs', selected, {priority: 'event'});
  });

  Shiny.addCustomMessageHandler(jobTableId + '_toggle_checkboxes', function(checked) {
    $('input.row_checkbox').prop('checked', checked);
  });
}

//Animation for loading screen
document.addEventListener("DOMContentLoaded", function () {
  const fadeSvgElements = document.querySelectorAll(".fade-svg");

  fadeSvgElements.forEach((el) => {
    el.style.opacity = 0;
    el.style.animationName = "fadeInOut";
    el.style.animationDuration = "2s";
    el.style.animationDelay = "0.2s";
    el.style.animationIterationCount = "infinite";
    el.style.animationTimingFunction = "ease-in-out";
    el.style.animationFillMode = "forwards";
  });
});