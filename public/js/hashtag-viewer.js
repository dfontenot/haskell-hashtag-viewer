var displayTimeMs = 2000; // how long to show each image
var curData = null;

var renderImage = function(data) {
  $('body').html("<img src='data:" + data.mimeType + ";base64," + data.image + "' />");
};

var renderNoImagesYet = function() {
  $('body').html("<p>No images yet</p>");
};

var showImageAndGetNext = function() {

  if (curData == null) {
    renderNoImagesYet();
  }
  else {
    renderImage(curData);
  }

  // get next image in background
  $.ajax({
    url: 'http://localhost:8000',
    type: 'GET',
    crossDomain: true,
    success: function(data) {
      curData = data;
    },
    error: function() {
      console.log("Issue loading the next image");
    }
  });

  // start timer again
  setTimeout(showImageAndGetNext, displayTimeMs);
};

$(document).ready(function() {
  setTimeout(showImageAndGetNext, displayTimeMs);
});
