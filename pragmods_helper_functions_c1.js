// ---------------- 1. HELPER FUNCTIONS ------------------
// This .js file has al the alphanumeric functions
// necessary to generate random instances of the experiment.

// random function
function random(a,b) {
    if (typeof b == "undefined") {
	a = a || 2;
	return Math.floor(Math.random()*a);
    } else {
	return Math.floor(Math.random()*(b-a+1)) + a;
    }
}

// range function
function range(a,b) {
    var rangeArray = new Array();

    for (var i=a; i<=b; i++) {
	rangeArray.push(i);
    }

    return rangeArray;
}

// unique function
function unique(arrayName)
{
    var newArray=new Array();
    label:for(var i=0; i<arrayName.length;i++ )
    {  
	for(var j=0; j<newArray.length;j++ )
	{
	    if(newArray[j]==arrayName[i]) 
		continue label;
	}
	newArray[newArray.length] = arrayName[i];
    }
    return newArray;
}

// shuffle function
function shuffle (a) 
{ 
    var o = [];
    for ( var i=0; i < a.length; i++) { o[i] = a[i]; }
    for (var j, x, i = o.length; i; j = parseInt(Math.random() * i), 
	 x = o[--i], o[i] = o[j], o[j] = x);
    return o;
}

// show slide function
function showSlide(id) {
    $(".slide").hide(); //jquery - all elements with class of slide - hide
    $("#"+id).show(); //jquery - element with given id - show
}


// create HTML for property matrix and base image
function stimHTML(base,n,prop_mat,props,id) {
    var html = "";

    html += '<img src="images2/' + base + '-base' + String(n+1) +
	'.png" width=200px height=200px alt="' + base + '" id="' + id + 'Image"/>';

    var c = 0;
    for (var p = 0; p < prop_mat.length; p++) {
	if (prop_mat[p] == 1) {
	    html += '<img  src="images2/' + base + '-' + props[p] + 
		'.png" width=200px height=200px alt="' + props[p] + '" ' +
		'id="' + id + 'Property' + String(c+1) + '"/>';
	    c = c + 1; // keep count of how many properties we've stacked
	}
    }

    return html;
}