// global
var breakage_starts_by_job_name = {};


function gen_broken_jobs_table(element_id, data_url) {

	var column_list = [
		{title: "Job", field: "job", width: 350},
		{title: "Build", field: "build", formatter: "link",
			formatterParams: {urlPrefix: "/build-details.html?build_id="},
			width: 75,
		},
		{title: "Flaky", field: "flaky", formatter:"tickCross", sorter:"boolean", width: 80},
		{title: "Broken", field: "known_broken", formatter:"tickCross", sorter:"boolean", width: 80},
	];

	var table = new Tabulator("#" + element_id, {
		height: 300,
		layout: "fitColumns",
		selectable: true,
		placeholder: "No Data Set",
		columns: column_list,
		ajaxURL: data_url,
		dataLoaded: function(data) {

			var rows = this.getRows();
			rows.forEach(function(row) {
				var row_data = row.getData();
				if (!(row_data["flaky"] || row_data["known_broken"])) {
					row.toggleSelect();
				}
			});
		},
	});

	return table;
}


function get_timeline_data(parms) {

	$("#scan-throbber").show();
	$.getJSON('/api/master-timeline', parms, function (mydata) {

		$("#scan-throbber").hide();
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

		{
			var node = document.createElement("span");

			var textnode = document.createTextNode("Mark failure end");
			node.appendChild(textnode);

			node.addEventListener("click", function () {mark_failure_resolution(commit_sha1, open_breakages);});

			context_menu_items.push(node);
		}

		{
			var node = document.createElement("span");

			var first_breakage_id = open_breakages[0]["start"]["db_id"];

			var linknode = document.createElement("a");
			linknode.setAttribute("href", "/breakage-details.html?cause=" + first_breakage_id);
			linknode.setAttribute("target", "_blank");

			var textnode = document.createTextNode("View cause details");
			linknode.appendChild(textnode);

			node.appendChild(linknode);

			context_menu_items.push(node);
		}
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
			var is_before_breakage_end = !("end" in job_breakage_span)
			                             || job_breakage_span.end == null
			                             || current_commit_index < job_breakage_span.end.record.payload.resolution_commit.db_id;

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
				render_timeline_table();
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
			selected_job_names.push(datum["job"]);
		}

		if (selected_job_names.length) {
			var jobs_list_delimited = selected_job_names.join(";");

			var description = $("#dialog-description-textarea").val();

			$.post({
				url: "/api/code-breakage-cause-report",
				data: {"sha1": commit_sha1, "description": description, "jobs": jobs_list_delimited},
				success: function( data ) {

					if (data.success) {
						alert("submitted report with ID: " + data.payload);
						render_timeline_table();
					} else {
						alert("Error: " + data.error.message);
					}
				}
			});

			document.getElementById("affected-jobs-dialog").close();

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

			var cell_value = get_cell_value_indirect(cell);

			var open_breakages = get_open_breakages(cell);
			const has_open_breakage = open_breakages.length > 0;

			const detected_contiguous_breakage = cell_value && cell_value.contiguous_breakage != null;


			if (has_open_breakage) {
				cell.getElement().style.backgroundColor = "#f664";
			}

			if (detected_contiguous_breakage) {
				// Repeat 3 times wthin a cell
				cell.getElement().style.backgroundSize = "33.3%";

				// vertical stripes
				var stripes_color_hex = "0000ff";
				var stripes_opacity_fraction = 0.7; // between 0 and 1
				cell.getElement().style.backgroundImage = "url(\"data:image/svg+xml,%3Csvg width='40' height='1' viewBox='0 0 40 1' xmlns='http://www.w3.org/2000/svg'%3E%3Cpath d='M0 0h20v1H0z' fill='%23" + stripes_color_hex + "' fill-opacity='" + stripes_opacity_fraction + "' fill-rule='evenodd'/%3E%3C/svg%3E\")";
			}

			var context_menu_items = get_context_menu_items_for_cell(cell);
			if (context_menu_items.length > 0) {
				cell.getElement().style.cursor = "context-menu";
			}


			if (cell_value != null) {
				var img_path = cell_value.is_flaky ? "yellow-triangle.svg"
					: cell_value.failure_mode["tag"] == "FailedStep" && cell_value.failure_mode["step_failure"]["tag"] == "Timeout" ? "purple-circle.svg"
						: cell_value.failure_mode["tag"] == "FailedStep" && cell_value.failure_mode["step_failure"]["tag"] == "NoMatch" ? "blue-square.svg"
							: cell_value.failure_mode["tag"] == "NoLog" ? "gray-diamond.svg"
								: cell_value.failure_mode["tag"] == "Success" ? "green-dot.svg" : "red-x.svg";

				var build_id = cell_value["build"]["build_id"];
				return link('<img src="/images/build-status-indicators/' + img_path + '" style="width: 100%; top: 50%;"/>', "/build-details.html?build_id=" + build_id);
			} else {
				return "";
			}
		},

		headerClick: function(e, column) {

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


function next_page() {

	var count = parseInt($('#count-input').val());
	var old_offset = parseInt($('#offset-input').val());

	$('#offset-input').val(old_offset + count);

	render_timeline_table();
}

var PULL_REQUEST_URL_PREFIX = "https://github.com/pytorch/pytorch/pull/";


function get_column_definitions(raw_column_list) {

	var legend_rows = [
		["Symbol", "Meaning"],
	];

	var symbol_pairs = [
		['yellow-triangle.svg', "flaky"],
		['red-x.svg',           "other match"],
		['blue-square.svg',     "no pattern match"],
		['purple-circle.svg',   "timeout"],
		['gray-diamond.svg',    "no log"],
		['green-dot.svg',       "success"],
	];

	for (var x of symbol_pairs) {
		legend_rows.push(['<img src="/images/build-status-indicators/' + x[0] + '" style="width: 20px"/>', x[1]]);
	}

	var legend = render_table(legend_rows, {"style": "vertical-align: bottom;"}, "Legend", true);

	var commit_column_definition = {
		title: 'Commit<br/>' + legend,
		field: "commit",
		headerVertical: false,
		formatter: function(cell, formatterParams, onRendered) {

			var commit_metadata = cell.getRow().getData()["commit_metadata"];

			var message_suffix = "";
			if (commit_metadata) {

				var pr_link = "";
				var matches_array = /https:\/\/github\.com\/pytorch\/pytorch\/pull\/(\d+)/.exec(commit_metadata["message"]);
				if (matches_array !== null) {
					var pr_number = matches_array[1];
					pr_link = render_tag("sup", render_tag("small", link("#" + pr_number, PULL_REQUEST_URL_PREFIX + pr_number)));
				}

				var message_subject = get_commit_subject(commit_metadata["message"]);
				message_suffix = pr_link + ": " + message_subject;
			}

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

	raw_column_list.sort();

	var column_list = [commit_column_definition].concat(generate_column_tree_base(raw_column_list));
	return column_list;
}


function generate_column_tree_base(column_names) {

	var collator = new Intl.Collator(undefined, {numeric: true, sensitivity: 'base'});
	column_names.sort(collator.compare);
	return generate_column_tree_recursive(column_names.map(x => [x, x]), 0);
}


function generate_column_tree_recursive(column_name_suffix_pairs, depth) {

	var colname_pair_by_prefix = {};
	for (var pair of column_name_suffix_pairs) {

		var col = pair[0];
		var suffix = pair[1];

		var chunks = suffix.split("_");
		var prefix = chunks.shift();

		var members = setDefault(colname_pair_by_prefix, prefix, []);
		members.push([col, chunks.join("_")]);
	}

	var column_list = [];

	for (var col_prefix in colname_pair_by_prefix) {

		var grouped_col_pairs = colname_pair_by_prefix[col_prefix];

		// < 3 leaves enough space to fit all header names horizontally
		if (grouped_col_pairs.length < 3) {

			for (var col_pair of grouped_col_pairs) {
				column_list.push(define_column(col_pair[0]));
			}

		} else {
			var subcolumn_definitions = generate_column_tree_recursive(grouped_col_pairs, depth + 1);

			// Collapse redundant headers
			var column_group_definition;

			if (subcolumn_definitions.length == 1) {
				column_group_definition = {
					title: col_prefix + "<br/>" + subcolumn_definitions[0]["title"],
					columns: subcolumn_definitions[0]["columns"],
				}
			} else {
				column_group_definition = {
					title: col_prefix,
					columns: subcolumn_definitions,
				}
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
		for (var affected_job_obj of breakage_span_obj.start.record.payload.affected_jobs) {
			setDefault(breakage_starts_by_job_name, affected_job_obj, []).push(breakage_span_obj);
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
	// FIXME somehow the callbacks accumulate in this dropdown
	// and result in multiple and irrelevant actions being triggered,
	// if the same cell is later clicked again.

	document.getElementById("myDropdown").classList.toggle("show");
}


function get_form_values() {

	var offset = $('#offset-input').val();
	var count = $('#count-input').val();

	var use_sha1_offset = false;
	var use_commit_index_bounds = false;

	var sha1 = $('#sha1-input').val() || "nothing";
	var min_commit_index = $('#commit-index-min').val() || 0;
	var max_commit_index = $('#commit-index-max').val() || 0;


	var pagination_mode = document.querySelector('input[name="pagination-mode"]:checked').value;

	if (pagination_mode == "start-by-sha1") {
		use_sha1_offset = true;
	}

	if (pagination_mode == "start-by-commit-index") {
		use_commit_index_bounds = true;
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

	return parms;
}


function render_timeline_table() {

	const parms = get_form_values();
	get_timeline_data(parms);
}


function url_from_form() {

	var url_parms;

	var pagination_mode = document.querySelector('input[name="pagination-mode"]:checked').value;

	if (pagination_mode == "start-by-sha1") {

		url_parms = {
			"count": $('#count-input').val(),
			"sha1": $('#sha1-input').val(),
		};

	} else if (pagination_mode == "start-by-commit-index") {

		url_parms = {
			"min_commit_index": $('#commit-index-min').val(),
			"max_commit_index": $('#commit-index-max').val(),
		};

	} else {

		url_parms = {
			"offset": $('#offset-input').val(),
			"count": $('#count-input').val(),
		};
	}

	document.location.search = $.param( url_parms );
}


function populate_form_from_url() {

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

	populate_form_from_url();


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

	render_timeline_table();
}

