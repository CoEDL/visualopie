document.getElementById("fileIn").addEventListener("change", function(e) {

  let files = e.target.files;
  let fullPath = files[0].webkitRelativePath
  var selectedPath = "Selected Folder: " + fullPath.substring(0, fullPath.lastIndexOf("/"));
  console.log(selectedPath);

  // var arr = new Array(files.length*2);
  // for (let i=0; i<files.length; i++) {
  //
  //     //console.log(files[i].webkitRelativePath);
  //     //console.log(files[i].name);
  //     arr[i] = files[i].webkitRelativePath;
  //     arr[i+files.length] = files[i].name;
  //
  //
  //     }

  Shiny.onInputChange("mydata", selectedPath);

});
