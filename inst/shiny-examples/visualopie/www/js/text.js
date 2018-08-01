window.pressed = function(){
        var a = document.getElementById('fileIn');
        if(a.value === "")
        {
            noFile.innerHTML = "No folder has been selected.";
        }
        else
        {
            noFile.innerHTML = "";
        }
    };
