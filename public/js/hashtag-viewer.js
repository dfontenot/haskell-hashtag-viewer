function onGetSuccess(data) {
  $('body').html("<img src='data:" + data.mimeType + ";base64," + data.image + "' />");
}

$(document).ready(function() {
  var request = $.ajax({
    url: "http://localhost:8000",
    type: "GET",
    crossDomain: true,
    success: onGetSuccess,
    error: function() { console.log("failure") }
  });
});
