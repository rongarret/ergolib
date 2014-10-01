// Drag and Drop file upload
// Based on:
// http://www.smilingsouls.net/Blog/20110413023355.html
// http://www.smilingsouls.net/dnd.html

var ergo = ergo || {};
ergo.dnd = { url : '/file-upload' };

(function() {

  function stopTheBuckHere(e) {
    e.preventDefault();
    e.stopPropagation();
  }

  function dragEnter(e) {
    stopTheBuckHere(e);
    $('div#uploadProgress').hide();
    $('div#uploadContainer').show();
  }

  function dragLeave(e) {
    stopTheBuckHere(e);
    $('div#uploadContainer').hide();
  }
  
  function drop(e) {
    stopTheBuckHere(e);
    $('div#uploadProgress').show();
    handle_file_drop(e);
  }

  function init() {
    $(document.body).bind('dragenter', dragEnter);
    $('div#uploadOverlay').bind('dragover', stopTheBuckHere);
    $('div#uploadOverlay').bind('dragleave', dragLeave);
    $('div#uploadOverlay').bind('drop', drop);
    console.log("Drag and drop initialized");
  }

  function showFiles(files) {
    $('div#uploadMessage').append("Uploading:<BR>");
    var cnt=files.length;
    var n = Math.min(10, cnt);
    for (var i = 0; i<n; i++) {
      var file = files[i];
      $('div#uploadMessage').append(
	file.name + ' (' + formatSize(file.size) + ')<br>');
    }
    if (cnt>n) {
      $('div#uploadMessage').append("And " + (cnt-n) + " more...");
    }
  }

  function handle_file_drop(e) {
    var files = e.originalEvent.dataTransfer.files;
    if (files.length) {
      stopTheBuckHere(e);
      showFiles(files);
      upload_files(files);
    } else {                     // Something other than a file was dropped
      $('div#uploadContainer').hide();
      alert("Drag and drop files from your desktop to upload them");
    }
  }
  
  function upload_files(files) {
    var form = new FormData();
    for (var i = 0; i < files.length; i++) form.append('dnd_file', files[i]);
    form.append('referrer', document.location);
    upload_form(form);
  }

  function pre_upload() {
  }
  
  function showProgress(e){
    if(e.lengthComputable){
      var n = Math.round(100*e.loaded/e.total);
      if (n<100) {
	$('div#uploadProgress > div > div').css('width', n + '%');
      } else {
	// NOTE: This can get called more than once
	$('div#uploadProgress').hide();
	$('div#uploadMessage').html('Done');
      }
    }
  }

  function upload_done(s) {
    $('div#uploadMessage').html('');
    $('div#uploadContainer').hide();
    if (s=="OK") {
      alert("Upload successful");
    } else if (s[0]=='/') {
      document.location=s;
    } else {
      // Error
      document.body.innerHTML = s;
    }
  }

  function upload_failed(e) {
    $('div#uploadContainer').hide();
    alert("File upload failed");
  }

  function upload_form(form) {
    $.ajax({
      url: ergo.dnd.url,
      type: 'POST',
      xhr: function() {  // custom xhr
	myXhr = $.ajaxSettings.xhr();
	if(myXhr.upload) { // check if upload property exists
          myXhr.upload.addEventListener('progress', showProgress, false);
	}
	return myXhr;
      },
      //Ajax events
      beforeSend: pre_upload,
      success: upload_done,
      error: upload_failed,
      data: form,
      //Options to tell JQuery not to process data or worry about content-type
      cache: false,
      contentType: false,
      processData: false
    });
  }
  
  function formatSize(bytes) {
    switch (true) {
    case (bytes < Math.pow(2,10)): {
      return bytes + ' Bytes';
    };
    case (bytes >= Math.pow(2,10) && bytes < Math.pow(2,20)): {
      return Math.round(bytes / Math.pow(2,10)) +' KB';
    };
    case (bytes >= Math.pow(2,20) && bytes < Math.pow(2,30)): {
      return Math.round((bytes / Math.pow(2,20)) * 10) / 10 + ' MB';
    };
    case (bytes > Math.pow(2,30)): {
      return Math.round((bytes / Math.pow(2,30)) * 100) / 100 + ' GB';
    };
    }
  }

  ergo.dnd.init = init;

})();

$(function() {
  if (typeof(FormData) != 'undefined') ergo.dnd.init();
  else alert('Your browser does not support standard HTML5 Drag and Drop');
});
