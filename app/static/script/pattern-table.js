
function post_modification(api_endpoint, data_dict) {

	$.post( {
		url: api_endpoint,
		data: data_dict,
		success: function( data ) {
			if (data.success) {
				console.log("Result: " + data.payload);
				location.reload();
			} else {
				if (data.error.details.authentication_failed) {
					alert("Not logged in: " + data.error.message);
					window.location.href = data.error.details.login_url;
				} else if (data.error.details.database_failed) {
					alert("Database error: " + data.error.message);
				} else {
					alert("Unknown error: " + data.error.message);
				}
			}
		}
	});
}

function remove_pattern_tag(pattern_id, tag) {

	if (confirm("Remove tag \"" + tag + "\" from pattern " + pattern_id + "?")) {
		var data_dict = {"pattern_id": pattern_id, "tag": tag};
		post_modification("/api/pattern-tag-remove", data_dict);
	}
}


function add_tag(pattern_id) {

	var prompt_val = prompt("Enter tag name (lowercase, no spaces):");
	if (prompt_val) {
		var data_dict = {"pattern_id": pattern_id, "tag": prompt_val};
		post_modification("/api/pattern-tag-add", data_dict);
	}
}


function update_description(pattern_id, new_value) {
	var data_dict = {"pattern_id": pattern_id, "description": new_value};
	post_modification("/api/pattern-description-update", data_dict);
}

function update_specificity(pattern_id, new_value) {
	var data_dict = {"pattern_id": pattern_id, "specificity": new_value};
	post_modification("/api/pattern-specificity-update", data_dict);
}



function render_relative_time(cell) {
	var val = cell.getValue();
	return val!= null ? moment(val).fromNow() : "never";
}


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
/*
		rowClick: function(e, row) {
			var pattern_id = row.getData()["id"];
			window.location.href = "/pattern-details.html?pattern_id=" + pattern_id;

		},
*/
		layout:"fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title:"Tags", field: "tags", sorter: "string",
				formatter: function(cell, formatterParams, onRendered) {
				        var tag_list = cell.getValue();
					var pattern_id = cell.getRow().getData()["id"];

					var tag_elements = tag_list.map(function(val) {
						var class_list = ["tag"];
						if (TAG_CLASSES.has(val)) {
							class_list.push("tag-class-" + val);
						}

						return "<span onclick='remove_pattern_tag(" + pattern_id + ",\"" + val + "\");' class='" + class_list.join(" ") + "'>" + val + "</span>";
					});

					tag_elements.push("<button class='tag-add-button' style='display: none;' id='tag-add-button-" + pattern_id + "' onclick='add_tag(" + pattern_id + ");'>+</button>");
					return tag_elements.join(" ");
				},
				cellMouseEnter: function(e, cell) {
					var pattern_id = cell.getRow().getData()["id"];
					$("#tag-add-button-" + pattern_id).show();

				},
				cellMouseLeave: function(e, cell) {
					var pattern_id = cell.getRow().getData()["id"];
					$("#tag-add-button-" + pattern_id).hide();
				},
			},
			{title:"Steps", field:"steps", sorter:"string"},
			{title:"Regex?", field:"is_regex", align:"center", formatter:"tickCross", sorter:"boolean", formatterParams: {crossElement: false}, width: 75},
			{title:"Pattern", field:"pattern", sorter:"string", widthGrow: 3,
				cssClass: "pattern-expression",
				formatter: "link",
				formatterParams: {
					urlPrefix: "/pattern-details.html?pattern_id=",
					urlField: "id",
				},
		        },
			{title:"Description", field:"description", sorter:"string",
				widthGrow: 2,
				editor: "input",
				cellEdited: function(cell) {
					var pattern_id = cell.getRow().getData()["id"];
					var new_description = cell.getValue();
					update_description(pattern_id, new_description);
				},
			},
			{title:"Count", field:"frequency", sorter:"number", align:"center", width: 75},
// BUG: The built-in "datetimediff" Tabulator formatter does not account for timezone properly.
//			{title:"Since", field:"earliest", sorter:"datetime", align:"center", formatter: "datetimediff", formatterParams: {humanize: true, suffix: true, invalidPlaceholder: "never", date: moment()}},
//			{title:"Until", field:"last", sorter:"datetime", align:"center", formatter: "datetimediff", formatterParams: {humanize: true, suffix: true, invalidPlaceholder: "never", date: moment()}},
			{title:"Since", field:"earliest", sorter:"datetime", align:"center", formatter: function(cell, formatterParams, onRendered) {
				return render_relative_time(cell)
			    }
			},
			{title:"Until", field:"last", sorter:"datetime", align:"center", formatter: function(cell, formatterParams, onRendered) {
				return render_relative_time(cell);
			    }
			},
			{title:"Specificity", field:"specificity", sorter:"number", align:"center", width: 100,
				editor:"number",
				editorParams:{
				    "min": 1,
				    "max": 100,
				},
				cellEdited: function(cell) {
					var pattern_id = cell.getRow().getData()["id"];
					var new_specificity = cell.getValue();
					update_specificity(pattern_id, new_specificity);
				},
			},
			{title:"Scanned percent", field:"percent_scanned", align:"center", sorter:"number"},
		],
		ajaxURL: api_endpoint_url,
	});
}
