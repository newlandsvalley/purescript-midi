

function loadBinaryFileImpl(onSuccess) {
    return function () {
      // console.log("inside load text file effects function ");
      var selectedFile = document.getElementById('fileinput').files[0];
      var reader = new FileReader();

      reader.onload = function(event) {
        var contents = event.target.result;
        var filespec = {contents:contents, name:selectedFile.name};
        // console.log("reader.onload File contents: " + contents);
        onSuccess (filespec)();
      };

      if (typeof selectedFile != 'undefined') {
         reader.readAsBinaryString(selectedFile);
      }
    }
}

exports.loadBinaryFileImpl = loadBinaryFileImpl;
