

// Initialization:
var jQT = $.jQTouch({
    icon: 'icon.png',
    statusBar: 'black'
});


// Extra library functions
// ================================================================================

$(document).ready(function(){
    $('#settings form').submit(saveSettings);
    $('#settings').bind('pageAnimationStart', loadSettings);

    $('#settings').bind('resetaction', resetSettings);

    $('#startSearch form').submit(sendSearch);
});

function saveSettings() {
    localStorage.dateformat = $('#dateformat').val();
    localStorage.username   = $('#username').val();
    localStorage.searchroot = $('#searchroot').val();
    localStorage.wholefile  = $('#wholefile').val();

    jQT.goBack();
    return false;
}

function resetSettings() {
    if ( confirm('Are you sure you want to restore the settings? ') ) 
    {
       delete localStorage.dateformat;
       delete localStorage.username;
       delete localStorage.searchroot;
       delete localStorage.wholefile; // undefined or "on"
       loadSettings();

       // I tried and failed to hack something that would refresh the page in-place:

       // Goes back to the home screen, full reload:
       location.reload(true);

       // FAIL:
       // doNavigation(hist[0].page, hist[0].page, "flip", true);
       // jQT.goTo(hist[0].page, "flip", false);

       // FAIL:
       // jQT.goTo("#settings", false, false);
       
       // Fails to properly reload values:
       // jQT.goBack();
       return false;
    }
}

function loadSettings() {
    $('#dateformat').val(localStorage.dateformat);
    $('#username').val(localStorage.username);
    $('#searchroot').val(localStorage.searchroot);
    $('#wholefile').val(localStorage.wholefile);
    // confirm('Settings loaded');
4}

function myjstest() {
    document.write('<p> Test of local storage, looking up field1: ', localStorage.testfield, '</p>');
    document.write('<p> Now an ABSENT field: ', localStorage.blahfield, '</p>');
}

var global_searchterms;


function stripHeadersHack( str ) {
    // Simple hack for now -- I don't know the right way -- drop lines until we see one
    // containing '{'.
    
    var arr = str.split('\n');
    var len = arr.length;
    var i = 0;
    while ( arr[i].indexOf('{') == -1  && i < len ) {
        i++;
    }

    // for(var j=0; j<i; j++) {   delete arr[j];    }
    if (i === len) 
    {
        alert("Bad response from server.  Apparently containing no JSON object.");
        return false;
    }
    else 
    {
        var pruned = arr.slice(i);
        var final = pruned.join('\n');
        return final;
    }
}

// This function dynamically produces a series of list elements.
function generateResults() {

    // Here we DYNAMICALLY create the searchResults page:
    // document.write('<div id="searchResults">' + XBg
    //     '<div class="toolbar">' +  
    //     '<h1>Search Results</h1>'+ 
    //     '<a class="button back" href="#">Back</a>'+ 
    //     '</div>'+
    //     '<ul class="edgetoedge">'+
   	//     '<li class="arrow"><a id="0" href="#viewResult">Result one</a></li>'+
    //     '<li class="arrow"><a id="1" href="#viewResult">Result two</a></li>'+
	//     '<li class="arrow"><a id="2" href="#viewResult">Result three</a></li>'+
    //     '</ul>'+
    //                '</div>');

    // document.write('<li class="arrow"><a id="0" href="#viewResult">Result 1</a></li>');
    // // confirm(global_searchterms.split(' '));
	// document.write('<li class="arrow"><a id="1" href="#viewResult">Result 2</a></li>');
	// document.write('<li class="arrow"><a id="2" href="#viewResult">Result 3</a></li>');

    var results = '';
    var termArray = global_searchterms.split(' ');

    var got_result = false;
    // var url = "http://wasp.ffh.us:6003/";
    var url = "http://localhost:6003/";

    results += '<li class="arrow"><a id="0" href="#viewResult"> dummy result </a></li>';

    // Use jQuery to get search results from the remote server:

    // This one works but there is a parsing job:
    //--------------------------------------------------------------------------------
    $.get(url, { name: "aUser", terms: global_searchterms },
      function(data){
         got_result = true;
         alert("Data Loaded, type "+ typeof(data)  +":\n " + data);
         
         var stripped = stripHeadersHack(data);
         var parsed = jQuery.parseJSON( stripped );
         // var parsed = JSON.parse( stripped); // This seems to work too.
         // alert("Parsed: " + typeof(parsed) +" "+ parsed);
         alert("Parsed fields: "+ parsed.date +" "+ parsed.header +" "+ parsed.body );

         document.getElementById("searchResultsContent").innerHTML = results;
      }, "html");
    //--------------------------------------------------------------------------------


    //--------------------------------------------------------------------------------
    // alert('blah');  
    // var req = new XMLHttpRequest();
    // req.open('GET', url, false); 
	// req.setRequestHeader('User-Agent','XMLHTTP/1.0');
    // req.send(null);
    // var headers = req.getAllResponseHeaders().toLowerCase();
    // alert(headers);  
    //--------------------------------------------------------------------------------


    // THIS ONE ISNT WORKING:
    //--------------------------------------------------------------------------------
    // $.getJSON( url, 
    //            { name: "aUser", terms: global_searchterms },
    //            function(data) 
    // {
    //     alert("Data Loaded:\n " + data);

    //     var items = [];

    //     $.each(data, function(key, item) 
    //     {
    //         results += '<li class="arrow"><a id="0" href="#viewResult">' + key + '</a></li>';
    //     });

    //     // $.each(data, function(key, val) {
    //     //   items.push('<li id="' + key + '">' + val + '</li>');
    //     // });

    //     // $('<ul/>', {
    //     //   'class': 'my-new-list',
    //     //   html: items.join('')
    //     // }).appendTo('body');

    //     document.getElementById("searchResultsContent").innerHTML = results;
    // });
    //--------------------------------------------------------------------------------

    // ----------------------------------------------------------------------------------------------------

    // HMM, I get the below error and THEN I get the data loaded... it must be asynchronous.
    // if (got_result) 
    //      alert("Done fetching results!");
    // else alert("ERROR: Could not get result from server.");

    // TEMP:
    // termArray.forEach(function(item) {
    //     results += '<li class="arrow"><a id="0" href="#viewResult">' + item + '</a></li>';
    // });
}

function sendSearch() {
    // Grab the search terms:
    var tmp = $('#searchterms').val();

    global_searchterms = tmp;

    generateResults();
    jQT.goTo("#searchResults", false, false);
}

// Do an http request to get the results:
// ====================================================================================================
// $.get(
//     "somepage.php",
//     {paramOne : 1, paramX : 'abc'},
//     function(data) {
//        alert('page content: ' + data);
//     }
// );

// $.get("test.cgi", { name: "John", time: "2pm" },
//    function(data){
//      alert("Data Loaded: " + data);
//    });

// Someone claims jquery is not necessary that this is builtin:
// function httpGet(theUrl)
//     {
//     var xmlHttp = null;

//     xmlHttp = new XMLHttpRequest();
//     xmlHttp.open( "GET", theUrl, false );
//     xmlHttp.send( null );
//     return xmlHttp.responseText;
//     }
// ====================================================================================================
