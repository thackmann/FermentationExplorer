// Jump to tab selected with buttons on home page
//Also close navigation bar menu
shinyjs.goToTab = function(tabName) {
        $('a[data-value=\"' + tabName + '\"]').click();
        setTimeout(function() {
          $('a[data-value=\"' + tabName + '\"]').closest('.dropdown-menu').removeClass('show');
        }, 0); // Delay in milliseconds to close the dropdown
      }
	  
// Function to dynamically resize width based on height
shinyjs.resizeWidthFromHeight = function(containerId, heightToWidthRatio) {
    function resizeContainer() {
        var height = $('#' + containerId).height();
        var newWidth = height * heightToWidthRatio; // Adjust the multiplier to your desired ratio
        $('#' + containerId).css('width', newWidth + 'px');
    }

    resizeContainer();  // Initial resize
    $(window).resize(resizeContainer);  // Resize on window resize
}