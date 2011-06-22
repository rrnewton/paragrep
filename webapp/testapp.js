
/*
 TODO LIST:
   (*) Reset the Note textarea upon successful submission.
   (*) Likewise, remove the previous success message when the submit button is pressed.
   (*) Highlight output
   (*) Show metadata (source file/line, length, etc)

 */


// Initialization:
var jQT = $.jQTouch({
    icon: 'icon.png',
    statusBar: 'black'
});

var debug_mode = false;
var global_searchterms;
var global_newEntryText;
var global_result_array;

// Bind event handlers:
$(document).ready(function(){
    $('#settings form').submit(saveSettings);
    $('#settings').bind('pageAnimationStart', loadSettings);

    $('#settings').bind('resetaction', resetSettings);

    //    $('#startSearch form').submit(sendSearch);
    $('#home form').submit(sendSearch);

    $('#newEntry form').submit(sendNewEntry);

});


// Extra library functions
// ================================================================================

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
}

function myjstest() {
    document.write('<p> Test of local storage, looking up field1: ', localStorage.testfield, '</p>');
    document.write('<p> Now an ABSENT field: ', localStorage.blahfield, '</p>');
}


// This version is only used to scroll forward until the JSON begins:
function stripHeadersHack( str ) {
    // Simple hack for now -- I don't know the right way -- drop lines until we see one
    // containing '{'.

    var arr = str.split('\n');
    var len = arr.length;
    var i = 0;
    while ( arr[i].indexOf('{') == -1  && i < len ) {
        i++;
    }

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

// Make an entry with the appropriate information.
function makeSearchHitListEntry( i, str )
{
    var result = "";
    result += ('<li class="arrow"><a id="'+i+'" href="javascript:viewFull('+i+')">'
               // + str.header +"  |  "+ str.body +'</a></li>'
	       + str.header +'</a></li>'
	       );
    return result;
}


function determineURL() {
    var port = window.location.port; // Leave it the same.
    // var port = 6003;
    // var port = 8080;
    var url = "http://localhost:"+port+"/";
    if (  window.location.protocol === "http:" &&
          ! (window.location.hostname === "") )
    {
        url = "http://" + window.location.hostname + ":"+port+"/";
    }
    return url;
}

// This function dynamically produces a series of list elements.
function generateResults() {

    var results = '';
    var termArray = global_searchterms.split(' ');

    var url = determineURL();

    //    results += '<li class="arrow"><a id="blah" href="#searchResults"> Current site URL: '+ document.URL +'</a></li>';
    results += '<li class="sep">Loading results from: '+ url +'</li>';
    document.getElementById("searchResultsContent").innerHTML = results;


    // Use jQuery to get search results from the remote server:
    //--------------------------------------------------------------------------------
    $.get(url, { name: "aUser"
	       , command: "search"
	       , terms: global_searchterms },
	  function(data) {
         if(debug_mode) alert("Data Loaded, type "+ typeof(data)  +":\n <" + data + ">");

         var stripped = stripHeadersHack(data);
         var parsed = jQuery.parseJSON( stripped );
         global_result_array = parsed.resultArray;
         // alert("Parsed: "+ parsed );
         // alert("Parsed fields: "+ parsed.header +" "+ parsed.body );

         var i = 0;
         $.each(global_result_array, function(key, item)
         {
             // alert("Typeof "+ typeof(item) + ".  Item itself: "+ item);
             results += makeSearchHitListEntry(i, item);
             // results += '<li class="arrow"><a id="0" href="#viewResult">' + item +'</a></li>';
             i++;
         });

         if (global_result_array.length === 0)
            results += '<li class="sep">No matches found!</li>';

         document.getElementById("searchResultsContent").innerHTML = results;
      }, "html");
}

function sendSearch() {
    // Grab the search terms:
    var tmp = $('#searchterms').val();

    global_searchterms = tmp;

    generateResults();
    // builtin animations: slide, slideup, dissolve, fade, flip, pop, swap, and cube
    jQT.goTo("#searchResults", 'slideup', false);
}

function viewFull(ind) {
    var entry = global_result_array[ind];
    // Mutate the target page so it has the desired contents:
    document.getElementById("viewFullContent").innerHTML = entry.body;
    // I can't seem to get these to work:  pop, swap, cube
    // jQT.goTo("#viewFull", 'ul li a', false);
    jQT.goTo("#viewFull", 'dissolve', false);
}


function sendNewEntry() {
    global_newEntryText = $('#newEntryText').val();
    var url = determineURL();

    var response = '<li class="sep">Server responded with:</li>';
    document.getElementById("newEntryServerResponse").innerHTML = response;

    $.get(url, { name: "aUser"
	           , command: "create"
	           , terms: "" 
               , body: global_newEntryText },

	  function(data) {
         if(debug_mode) alert("Server responded to new note with "+ typeof(data)  +":\n <" + data + ">");

         // For some reason I'm not getting the same headers here.  Weird:
         // var stripped = stripHeadersHack(data);
         // var parsed = jQuery.parseJSON( stripped );
         // alert("Got stripped/parsed: " + parsed);

         response += '<li class="sep"> '+ data +' </li>';
         document.getElementById("newEntryServerResponse").innerHTML = response;
      }, "html");

    // jQT.goBack();
}
