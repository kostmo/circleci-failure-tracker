function submit_rescan() {
    start_rescan();

    return false;
}


var scan_progress_content = [];

function fetch_scan_progress() {

   $.getJSON('/api/get-scan-progress', {"offset": scan_progress_content.length}, function (data) {

        console.log("my data: " + data);


        var status_elem = getElementById("status_elem"):

        if (data.success) {

            scan_progress_content += data.lines

            setTimeout(fetch_scan_progress, 1000);
        } else {

        }
   });
}


function clear_array(array) {
  while (array.length) {
    array.pop();
  }
}



function start_rescan() {

   clear_array(scan_progress_content);

   $.getJSON('/api/start-scan', function (data) {


        fetch_scan_progress();
   });
}
