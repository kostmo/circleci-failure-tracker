// global
var breakage_starts_by_job_name = {};


function gen_broken_jobs_table(element_id, data_url) {

	var column_list = [
		{title: "Job", field: "job", width: 350},
		{title: "Build", field: "build", formatter: "link",
			formatterParams: {urlPrefix: "/build-details.html?build_id="},
			width: 75,
		},
	];

	var table = new Tabulator("#" + element_id, {
		height: 300,
		layout: "fitColumns",
		selectable: true,
		placeholder: "No Data Set",
		columns: column_list,
		ajaxURL: data_url,
	});

	return table;
}


function get_timeline_data(offset, count) {

	var urlParams = new URLSearchParams(window.location.search);

	var sha1 = "nothing";
	var use_sha1_offset = false;
	var use_commit_index_bounds = false;

	var url_sha1 = urlParams.get('sha1');

	var min_commit_index = urlParams.get('min_commit_index');
	var max_commit_index = urlParams.get('max_commit_index');

	if (url_sha1 != null) {
		sha1 = url_sha1;
		use_sha1_offset = true;

	}


	if (min_commit_index != null && max_commit_index != null) {
		use_commit_index_bounds = true;
	} else {
		min_commit_index = 0;
		max_commit_index = 0;
	}

	var parms = {
		"offset": offset,
		"sha1": sha1,
		"count": count,
		"use_sha1_offset": use_sha1_offset,
		"use_commit_index_bounds": use_commit_index_bounds,
		"min_commit_index": min_commit_index,
		"max_commit_index": max_commit_index,
	}


	$.getJSON('/api/master-timeline', parms, function (mydata) {

		if (mydata.success) {

			gen_timeline_table("master-timeline-table", mydata.payload);

		} else {
			alert(mydata.error);
		}
	});
}


function get_context_menu_items_for_cell(cell) {

	var job_name = cell.getColumn().getField();
	var commit_sha1 = cell.getRow().getData()["commit"];

	var context_menu_items = [];

	var cell_value = get_cell_value_indirect(cell);
	if (cell_value != null) {
		var node = document.createElement("span");
		var textnode = document.createTextNode("Mark failure start");
		node.appendChild(textnode);

		node.addEventListener("click", function () {mark_failure_cause(commit_sha1, job_name);});

		context_menu_items.push(node);
	}

	var open_breakages = get_open_breakages(cell);
	if (open_breakages.length > 0) {

		var node = document.createElement("span");

		var textnode = document.createTextNode("Mark failure end");
		node.appendChild(textnode);

		node.addEventListener("click", function () {mark_failure_resolution(commit_sha1, open_breakages);});

		context_menu_items.push(node);
	}

	return context_menu_items;
}


function get_open_breakages(cell) {

	var open_breakages = [];

	var job_name = cell.getColumn().getField();

	if (job_name in breakage_starts_by_job_name) {
		var current_commit_index = cell.getRow().getData()["commit_index"];

		for (var job_breakage_span of breakage_starts_by_job_name[job_name]) {

			var is_after_breakage_start = current_commit_index >= job_breakage_span.start.record.payload.breakage_commit.db_id;
			var is_before_breakage_end = !("end" in job_breakage_span) || job_breakage_span.end == null || current_commit_index < job_breakage_span.end.record.payload.resolution_commit.db_id;

			if (is_after_breakage_start && is_before_breakage_end) {

				open_breakages.push(job_breakage_span);
			}
		}
	}

	return open_breakages;
}


function mark_failure_resolution(commit_sha1, active_breakages) {

	console.log("Submitting resolution report. Active breakage count: " + active_breakages.length);

	var cause_ids = [];
	for (var breakage_obj of active_breakages) {
		cause_ids.push(breakage_obj.start.db_id);
	}

	var causes_delimited = cause_ids.join(";")

        $.post({
		url: "/api/code-breakage-resolution-report",
		data: {"sha1": commit_sha1, "causes": causes_delimited},
		success: function( data ) {

			if (data.success) {
				alert("submitted report with ID: " + data.payload);
				render_table();
			} else {
				alert("Error: " + data.error.message);
			}
		}
        });
}



function mark_failure_cause(commit_sha1, clicked_job_name) {

	var tabulator = gen_broken_jobs_table("broken-jobs-table", "/api/list-commit-jobs?sha1=" + commit_sha1);

	$('#dialog-cancel-button').click(function(e) {
		document.getElementById("affected-jobs-dialog").close();
	});

	$('#dialog-select-all-button').click(function(e) {

		var rows = tabulator.getRows();
		rows.forEach(function(row) {
			row.toggleSelect();
		});
	});

	$('#dialog-submit-button').click(function(e) {

		var selectedData = tabulator.getSelectedData();

		console.log("Selected count: " + selectedData.length);
		var selected_job_names = [];
		for (var datum of selectedData) {

			console.log("Row: " + JSON.stringify(datum));

			selected_job_names.push(datum["job"]);
		}

		if (selected_job_names.length) {
			var jobs_list_delimited = selected_job_names.join(";");

			var description = $("#dialog-description-textarea").val();

			if (description) {
				$.post({
					url: "/api/code-breakage-cause-report",
					data: {"sha1": commit_sha1, "description": description, "jobs": jobs_list_delimited},
					success: function( data ) {

						if (data.success) {
							alert("submitted report with ID: " + data.payload);
							render_table();
						} else {
							alert("Error: " + data.error.message);
						}
					}
				});

				document.getElementById("affected-jobs-dialog").close();

			} else {
				alert("Description must not be empty!");
			}

		} else {
			alert("Must select at least one job!");
		}
	});

	document.getElementById("affected-jobs-dialog").showModal();
}


function get_cell_value_indirect(cell) {
	// XXX Some of the column names can contain periods, which inadvertently
	// indicates a nested JSON object (see http://tabulator.info/docs/4.0/columns#field-nesting)
	// There's not an obvious way to turn off this interpretation, so we access the data
	// in a different, more indirect way:
//	var cell_value = cell.getValue();

	var cell_value = cell.getRow().getData()[cell.getColumn().getField()];
	return cell_value;
}


function define_column(col) {
	var col_dict = {
		title: col,
		field: col,
		headerVertical: "flip",
		width: 22,
		minWidth: 22,
		resizable: false,
		headerSort: false,
		cssClass: "smallish",
		tooltip: function(cell) {

			var cell_value = get_cell_value_indirect(cell);
			if (cell_value != null) {
				var failure_mode_obj = cell_value["failure_mode"];
				if (failure_mode_obj["tag"] == "FailedStep" && failure_mode_obj["step_failure"]["tag"] == "PatternMatch") {

					return failure_mode_obj["step_failure"]["contents"]["line_text"];

				} else if (failure_mode_obj["tag"] == "FailedStep" && failure_mode_obj["step_failure"]["tag"] == "Timeout") {

					return "Timeout on step \"" + failure_mode_obj["step_name"] + "\"";

				} else {
					return false;
				}
			} else {
				return false;
			}
		},
		formatter: function(cell, formatterParams, onRendered) {

			var open_breakages = get_open_breakages(cell);
			if (open_breakages.length > 0) {
				cell.getElement().style.backgroundColor = "#fcca";
			}

			var context_menu_items = get_context_menu_items_for_cell(cell);
			if (context_menu_items.length > 0) {
				cell.getElement().style.cursor = "context-menu";
			}

			var cell_value = get_cell_value_indirect(cell);

			if (cell_value != null) {
				var img_path = cell_value.is_flaky ? "yellow-triangle.svg"
					: cell_value.failure_mode["tag"] == "FailedStep" && cell_value.failure_mode["step_failure"]["tag"] == "Timeout" ? "purple-circle.svg"
						: cell_value.failure_mode["tag"] == "FailedStep" && cell_value.failure_mode["step_failure"]["tag"] == "NoMatch" ? "blue-square.svg"
							: cell_value.failure_mode["tag"] == "NoLog" ? "gray-diamond.svg"
								: cell_value.failure_mode["tag"] == "Success" ? "green-dot.svg" : "red-x.svg";

				var build_id = cell_value["build"]["build_id"];
				return link('<img src="/images/' + img_path + '" style="width: 100%; top: 50%;"/>', "/build-details.html?build_id=" + build_id);
			} else {
				return "";
			}
		},

		headerClick: function(e, column){
			//e - the click event object
			//column - column component

			var columnField = column.getField();

			console.log("Column name: " + columnField);

			// TODO make header
/*
			const el = document.createElement('textarea');
			el.value = cell_value;
			el.setAttribute('readonly', '');
			el.style.position = 'absolute';
			el.style.left = '-9999px';
			document.body.appendChild(el);
			el.select();
			document.execCommand('copy');
			document.body.removeChild(el);
*/


		},
		cellContext: function(e, cell) {

			var dropdown_element = document.getElementById("myDropdown");

			while (dropdown_element.firstChild) {
				dropdown_element.removeChild(dropdown_element.firstChild);
			}

			var context_menu_items = get_context_menu_items_for_cell(cell);
			if (context_menu_items.length > 0) {

				dropdown_element.style.left = event.pageX;
				dropdown_element.style.top = event.pageY;


				for (var x of context_menu_items) {
					dropdown_element.appendChild(x);
				}

				showContextMenu();
				e.preventDefault();
			}
		},
	};

	return col_dict;
}


function get_column_definitions(raw_column_list) {

	var commit_column_definition = {
		title: 'Commit<br/><table style="vertical-align: bottom;"><caption>Legend</caption><tbody><tr><th>Symbol</th><th>Meaning</th></tr><tr><td><img src="/images/yellow-triangle.svg" style="width: 20px"/></td><td>flaky</td></tr><tr><td><img src="/images/red-x.svg" style="width: 20px"/></td><td>other match</td></tr><tr><td><img src="/images/blue-square.svg" style="width: 20px"/></td><td>no pattern match</td></tr><tr><td><img src="/images/purple-circle.svg" style="width: 20px"/></td><td>timeout</td></tr><tr><td><img src="/images/gray-diamond.svg" style="width: 20px"/></td><td>idiopathic</td></tr><tr><td><img src="/images/green-dot.svg" style="width: 20px"/></td><td>success</td></tr></tbody></table>',
		field: "commit",
		headerVertical: false,
		formatter: function(cell, formatterParams, onRendered) {

			var commit_metadata = cell.getRow().getData()["commit_metadata"];

			var message_suffix = commit_metadata ? ": " + commit_metadata["message"].split(/\r?\n/)[0] : "";
			return sha1_link(cell.getValue()) + message_suffix;
		},
		tooltip: function(cell) {

			var commit_metadata = cell.getRow().getData()["commit_metadata"];
			if (commit_metadata) {
				return commit_metadata["message"];
			} else {
				console.log("No message for commit " + cell.getValue());
				return false;
			}
		},
		minWidth: 90,
		width: 300,
		resizable: true,
		headerSort: false,
		frozen: true,
	};

//	var filtered_column_names = raw_column_list.filter(x => !x.startsWith("binary"));
	var filtered_column_names = raw_column_list;
	filtered_column_names.sort();

	var column_list = [commit_column_definition].concat(generate_column_tree(filtered_column_names));
	return column_list;
}


// TODO Make recursive
function generate_column_tree(column_names) {

	var colname_by_prefix = {};
	for (var col of column_names) {
		var chunks = col.split("_");

		var members = setDefault(colname_by_prefix, chunks[0], []);
		members.push(col);
	}

	var column_list = [];

	for (var col_prefix in colname_by_prefix) {

		var grouped_cols = colname_by_prefix[col_prefix];

		if (grouped_cols.length < 3) {

			for (var col of grouped_cols) {
				var col_dict = define_column(col);
				column_list.push(col_dict);
			}

		} else {

			var subcolumn_definitions = [];
			for (var col of grouped_cols) {
				var col_dict = define_column(col);
				subcolumn_definitions.push(col_dict);
			}

			var column_group_definition = {
				title: col_prefix,
				columns: subcolumn_definitions,
			}

			column_list.push(column_group_definition);
		}
	}

	return column_list;
}


function gen_timeline_table(element_id, fetched_data) {

	var column_list = get_column_definitions(fetched_data.columns);


	// global
	breakage_starts_by_job_name = {};
	for (var breakage_span_obj of fetched_data.breakage_spans) {

		var breakage_start_obj = breakage_span_obj.start;
		for (var affected_job_obj of breakage_start_obj.record.payload.affected_jobs) {

			var breakage_starts_for_job = setDefault(breakage_starts_by_job_name, affected_job_obj.payload, []);
			breakage_starts_for_job.push(breakage_span_obj);
		}
	}


	var build_failures_by_commit = {};
	for (var failure_obj of fetched_data.failures) {
		var failures_by_job_name = setDefault(build_failures_by_commit, failure_obj.build.vcs_revision, {});
		failures_by_job_name[failure_obj.build.job_name] = failure_obj;
	}

	var table_data = [];
	for (var commit_obj of fetched_data.commits) {
		var row_dict = {};

		var sha1 = commit_obj.record.commit;
		var failures_by_job_name = build_failures_by_commit[sha1] || {};

		row_dict["commit_index"] = commit_obj.db_id;
		row_dict["commit"] = sha1;
		row_dict["commit_metadata"] = commit_obj.record.metadata;

		for (var job_name in failures_by_job_name) {
			row_dict[job_name] = failures_by_job_name[job_name];
		}

		table_data.push(row_dict);
	}


	var table = new Tabulator("#" + element_id, {
		layout: "fitColumns",
		placeholder: "No Data Set",
//		selectable: true,
		columns: column_list,
		data: table_data,
	});
}



/* When the user clicks on the button, 
toggle between hiding and showing the dropdown content */
function showContextMenu() {
	document.getElementById("myDropdown").classList.toggle("show");
}


function render_table() {

	var offset = $('#offset-input').val();
	var count = $('#count-input').val();

	get_timeline_data(offset, count);
}


function init_fields() {

	var urlParams = new URLSearchParams(window.location.search);

	var offset = urlParams.get('offset');
	if (offset != null) {
		$('#offset-input').val(offset);
	}

	var count = urlParams.get('count');
	if (count != null) {
		$('#count-input').val(count);
	}


	var commit_index_min = urlParams.get('min_commit_index');
	if (commit_index_min != null) {
		$('#commit-index-min').val(commit_index_min);
	}

	var commit_index_max = urlParams.get('max_commit_index');
	if (commit_index_max != null) {
		$('#commit-index-max').val(commit_index_max);
	}



	var starting_sha1 = urlParams.get('sha1');
	if (starting_sha1 != null) {
		$('#sha1-input').val(starting_sha1);
	}



	var radios = $('input:radio[name=pagination-mode]');

	if (commit_index_min != null && commit_index_max != null) {

		radios.filter('[value=start-by-commit-index]').prop('checked', true);

	} else if (starting_sha1 != null) {

		radios.filter('[value=start-by-sha1]').prop('checked', true);

	} else {

		radios.filter('[value=start-by-offset]').prop('checked', true);
	}

}


function main() {

	init_fields();


	// Close the dropdown menu if the user clicks outside of it
	window.onclick = function(event) {
		if (!event.target.matches('.dropbtn')) {
			var dropdowns = document.getElementsByClassName("dropdown-content");
			for (var i = 0; i < dropdowns.length; i++) {
				var openDropdown = dropdowns[i];
				if (openDropdown.classList.contains('show')) {
					openDropdown.classList.remove('show');
				}
			}
		}
	}

	render_table();
}

