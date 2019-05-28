function gen_patterns_table(pattern_id, filtered_branches) {


	// Note the plural ("s")
	var api_endpoint_url = "/api/patterns";

	if (pattern_id != null) {
	        var query_parms = {
			"pattern_id": pattern_id,
		};

	        var ajax_url_query_string = $.param(query_parms);
		api_endpoint_url = "/api/pattern?" + ajax_url_query_string;

	} else if (filtered_branches.length > 0) {

	        var query_parms = {
			"branches": filtered_branches,
		};

	        var ajax_url_query_string = $.param(query_parms, true);
		api_endpoint_url = "/api/patterns-branch-filtered?" + ajax_url_query_string;
	}

        var height = pattern_id == null ? "400px" : null;

	var table = new Tabulator("#patterns-table", {
	    height: height,
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title:"Tags", field:"tags", sorter:"string"},
		{title:"Steps", field:"steps", sorter:"string"},
		{title:"Regex", field:"is_regex", align:"center", formatter:"tickCross", sorter:"boolean", formatterParams: {crossElement: false}, width: 75},
		{title:"Pattern", field:"pattern", sorter:"string", widthGrow: 3, formatter: function(cell, formatterParams, onRendered) {
			return "<code>" + cell.getValue() + "</code>";
		  },
                },
		{title:"Description", field:"description", sorter:"string", formatter: "link", formatterParams: {urlPrefix: "/pattern-details.html?pattern_id=", urlField: "id"}, widthGrow: 2},
		{title:"Frequency", field:"frequency", sorter:"number", align:"center", width: 75},
		{title:"Last Occurrence", field:"last", sorter:"datetime", align:"center"},
		{title:"Specificity", field:"specificity", sorter:"number", align:"center"},
		{title:"Scanned percent", field:"percent_scanned", align:"center", sorter:"number"},
	    ],
            ajaxURL: api_endpoint_url,
	});
}
