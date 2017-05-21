var displayTimeMs = 5000; // how long to show each image
var curData = null;

var renderImage = function(data) {
  $('#image-message').addClass('hidden');

  var newImage = $('<img/>').addClass('image-response image-initial-hidden')
    .attr('src', 'data:' + data.mimeType + ";base64," + data.image);
  newImage.fadeIn(displayTimeMs);

  $('#container').append(newImage);
  newImage.addClass('image-visible');

  if ($('#container').children().length > 2) {
    $('#container').find(':first-child').remove();
  }
};

var renderNoImagesYet = function() {
  $('#image-message').addClass('visible');
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
      console.error("Issue loading the next image");
    }
  });

  // start timer again
  setTimeout(showImageAndGetNext, displayTimeMs);
};

$(document).ready(function() {
  setTimeout(showImageAndGetNext, displayTimeMs);
});
