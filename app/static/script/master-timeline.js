// global
var breakage_starts_by_job_name = {};
var grid_table = null;


const MAX_HORIZONTAL_LETTERS = 5;
const FITTABLE_LETTERS_PER_COLUMN = 2; // empirically determined

// empirically, < 3 leaves enough space to fit all header names horizontally
const MIN_HEADER_GROUPING_COLUMNS = 3;


function gen_broken_jobs_table(element_id, data_url) {

	const column_list = [
		{title: "Job", field: "job", width: 350},
		{title: "Recurrences", field: "occurrence_count", width: 75},
		{title: "Build", field: "universal_build_id", formatter: "link",
			formatterParams: {urlPrefix: "/build-details.html?build_id="},
			width: 75,
		},
		{title: "Flaky", field: "flaky", formatter:"tickCross", sorter:"boolean", width: 80},
		{title: "Broken", field: "known_broken", formatter:"tickCross", sorter:"boolean", width: 80},
	];

	const table = new Tabulator("#" + element_id, {
		height: 300,
		layout: "fitColumns",
		selectable: true,
		placeholder: "No Data Set",
		columns: column_list,
		ajaxURL: data_url,
		dataLoaded: function(data) {

			const rows = this.getRows();
			rows.forEach(function(row) {
				const row_data = row.getData();
				if (!(row_data["flaky"] || row_data["known_broken"])) {
					row.toggleSelect();
				}
			});
		},
	});

	return table;
}


function get_timeline_data(parms) {

	const start_time = new Date();
	$("#scan-throbber").show();

	$.getJSON('/api/master-timeline', parms, function (mydata) {
		$("#scan-throbber").hide();

		const end_time = new Date();
		if (mydata.success) {

			const last_cache_update_utctime_tuple = mydata["payload"]["db_benchmarking"]["last_mview_update"];
			const last_cache_update_utctime = last_cache_update_utctime_tuple[0]
			console.log("Last update UTCTime: " + last_cache_update_utctime + " by " + last_cache_update_utctime_tuple[1]);

			$("#last-cache-update-time-container").html(moment(last_cache_update_utctime).fromNow());

			const time_diff = end_time.getTime() - start_time.getTime();
			const seconds_diff = time_diff / 1000;

			console.log("Fetched timeline data in " + seconds_diff.toFixed(1) + " seconds.");
			gen_timeline_table("master-timeline-table", mydata.payload);

		} else {
			alert("got an error: " + mydata.error);
		}
	});
}


function get_context_menu_items_for_cell(cell) {

	const job_name = cell.getColumn().getField();
	const commit_sha1 = cell.getRow().getData()["commit"];

	const context_menu_items = [];

	const cell_value = get_cell_value_indirect(cell);
	if (cell_value != null) {

		const selected_data = [] || (grid_table && grid_table.getSelectedData());

		if (selected_data.length > 1) {

			const node = document.createElement("span");
			const textnode = document.createTextNode("Mark failure span");
			node.appendChild(textnode);

			node.addEventListener("click", function () {mark_failure_span(selected_data);});

			context_menu_items.push(node);

		} else if (cell_value.failure_mode["tag"] != "Success") {

			const node = document.createElement("span");
			const textnode = document.createTextNode("Mark failure start");
			node.appendChild(textnode);

			node.addEventListener("click", function () {mark_failure_cause(commit_sha1);});

			context_menu_items.push(node);
		}
	}

	const open_breakages = get_open_breakages(cell);
	if (open_breakages.length > 0) {

		{
			const node = document.createElement("span");

			const textnode = document.createTextNode("Mark failure resolution");
			node.appendChild(textnode);

			node.addEventListener("click", function () {mark_failure_resolution(commit_sha1, open_breakages);});

			context_menu_items.push(node);
		}

		{
			const node = document.createElement("span");

			const first_breakage_id = open_breakages[0]["start"]["db_id"];

			const linknode = document.createElement("a");
			linknode.setAttribute("href", "/breakage-details.html?cause=" + first_breakage_id);
			linknode.setAttribute("target", "_blank");

			const textnode = document.createTextNode("View cause details");
			linknode.appendChild(textnode);

			node.appendChild(linknode);

			context_menu_items.push(node);
		}
	}

	return context_menu_items;
}


function get_open_breakages(cell) {

	const open_breakages = [];

	const job_name = cell.getColumn().getField();

	if (job_name in breakage_starts_by_job_name) {
		const current_commit_index = cell.getRow().getData()["commit_index"];

		for (var job_breakage_span of breakage_starts_by_job_name[job_name]) {

			const is_after_breakage_start = current_commit_index >= job_breakage_span.start.record.payload.breakage_commit.db_id;
			const is_before_breakage_end = !("end" in job_breakage_span)
			                             || job_breakage_span.end == null
			                             || current_commit_index < job_breakage_span.end.record.payload.resolution_commit.db_id;

			if (is_after_breakage_start && is_before_breakage_end) {
				open_breakages.push(job_breakage_span);
			}
		}
	}

	return open_breakages;
}


function update_subform_visibility(is_ongoing_checked) {
	$('#span-end-form-section').css('visibility', is_ongoing_checked ? 'hidden' : 'visible');
}


function refresh_cache() {

	$("#scan-throbber").show();
        $.post({
		url: "/api/refresh-materialized-view",
		success: function( data ) {
			$("#scan-throbber").hide();

			console.log("success");
			update_url_from_form();
		}
        });
}


function mark_failure_resolution(commit_sha1, active_breakages) {

	console.log("Submitting resolution report. Active breakage count: " + active_breakages.length);

	const cause_ids = active_breakages.map(breakage_obj => breakage_obj.start.db_id);
	const causes_delimited = cause_ids.join(";")

        $.post({
		url: "/api/code-breakage-resolution-report",
		data: {
			"sha1": commit_sha1,
			"causes": causes_delimited,
		},
		success: function( data ) {

			if (data.success) {
				alert("submitted report with ID: " + data.payload);
				update_url_from_form();
			} else {
				alert("Error: " + data.error.message);
			}
		}
        });
}


function mark_failure_span(selected_data) {

	const selected_commit_indices = [];
	const commit_sha1_by_index = {};

	console.log("Grid selected row count: " + selected_data.length);
	for (var datum of selected_data) {
		selected_commit_indices.push(datum["commit_index"]);
		commit_sha1_by_index[datum["commit_index"]] = datum["commit"];
	}


	console.log("Commit indices: " + selected_commit_indices);

	const min_commit_index = Math.min(...selected_commit_indices);
	const max_commit_index = Math.max(...selected_commit_indices);

	console.log("Min: " + min_commit_index + "; Max: " + max_commit_index);

	const parms_string = $.param({
		"first_index": min_commit_index,
		"last_index": max_commit_index,
	});


	$('#is-ongoing-checkbox').prop('checked', false);


	$("#breakage-span-start-commit").val( commit_sha1_by_index[min_commit_index] );
	$("#breakage-span-last-commit").val( commit_sha1_by_index[max_commit_index] );


	return mark_failure_cause_common("/api/list-master-commit-range-jobs?" + parms_string);
}


function mark_failure_cause(commit_sha1) {

	$("#breakage-span-start-commit").val(commit_sha1);

	return mark_failure_cause_common("/api/list-commit-jobs?sha1=" + commit_sha1);
}


function mark_failure_cause_common(api_url) {

	const is_ongoing_checked = $('#is-ongoing-checkbox').is(":checked");
	update_subform_visibility(is_ongoing_checked);

	const tabulator = gen_broken_jobs_table("broken-jobs-table", api_url);

	$('#dialog-cancel-button').click(function(e) {
		document.getElementById("affected-jobs-dialog").close();
	});

	$('#dialog-select-all-button').click(function(e) {
		const rows = tabulator.getRows();
		rows.forEach(function(row) {
			row.toggleSelect();
		});
	});


	$('#dialog-select-none-button').click(function(e) {
		const rows = tabulator.getRows();
		rows.forEach(function(row) {
			row.deselect();
		});
	});


	$("#mini-throbber-failure-modes").show();
	$.getJSON('/api/list-failure-modes', function (mydata) {
		$("#mini-throbber-failure-modes").hide();

		$('#breakage-mode-selector').empty();

		for (var item of mydata) {
			$('#breakage-mode-selector').append(render_tag("option", item["record"]["label"], {"value": item["db_id"]}));
		}
	});


	$('#dialog-submit-button').click(function(e) {

		const selectedData = tabulator.getSelectedData();

		console.log("Selected count: " + selectedData.length);
		const selected_job_names = selectedData.map(datum => datum["job"]);

		if (selected_job_names.length) {
			const jobs_list_delimited = selected_job_names.join(";");

			const notes = $("#dialog-description-textarea").val();
			const is_ongoing = $('#is-ongoing-checkbox').is(":checked");
			const breakage_mode_id = $("#breakage-mode-selector").val();

			const cause_sha1 = $("#breakage-span-start-commit").val();
			const last_affected_sha1 = $("#breakage-span-last-commit").val();

			$.post({
				url: "/api/code-breakage-cause-report",
				data: {
					"cause_sha1": cause_sha1,
					"notes": notes,
					"jobs": jobs_list_delimited,
					"failure_mode_id": breakage_mode_id,
					"is_ongoing": is_ongoing,
					"last_affected_sha1": last_affected_sha1,
				},
				success: function( data ) {

					if (data.success) {
						alert("submitted report with ID: " + data.payload);
						update_url_from_form();
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

	return cell.getRow().getData()[cell.getColumn().getField()];
}


function define_column(col) {
	const col_dict = {
		title: col,
		field: col,
		headerVertical: "flip",
		width: 22,
		minWidth: 22,
		resizable: false,
		headerSort: false,
		cssClass: "smallish",
		tooltip: function(cell) {

			const job_name = cell.getColumn().getField();
			const bracketed_job_name = "[" + job_name + "]\n";

			const cell_value = get_cell_value_indirect(cell);
			if (cell_value != null) {
				const failure_mode_obj = cell_value["failure_mode"];
				if (failure_mode_obj["tag"] == "FailedStep" && failure_mode_obj["step_failure"]["tag"] == "PatternMatch") {

					return bracketed_job_name + "<" + failure_mode_obj["step_name"] + ">\n" + failure_mode_obj["step_failure"]["contents"]["line_text"];

				} else if (failure_mode_obj["tag"] == "FailedStep" && failure_mode_obj["step_failure"]["tag"] == "Timeout") {

					return bracketed_job_name + "Timeout on step \"" + failure_mode_obj["step_name"] + "\"";

				} else if (failure_mode_obj["tag"] == "FailedStep" && failure_mode_obj["step_failure"]["tag"] == "NoMatch") {

					return bracketed_job_name + "<" + failure_mode_obj["step_name"] + ">";

				} else {
					return false;
				}
			} else {
				return false;
			}
		},
		formatter: function(cell, formatterParams, onRendered) {

			const cell_value = get_cell_value_indirect(cell);

			const open_breakages = get_open_breakages(cell);
			const has_open_breakage = open_breakages.length > 0;

			const detected_contiguous_breakage = cell_value && cell_value.detected_breakages.longitudinal_breakage != null;
			const detected_lateral_breakage = cell_value && cell_value.detected_breakages.lateral_breakage != null;

			if (has_open_breakage) {
				cell.getElement().style.backgroundColor = "#f664";
			}


			if (detected_contiguous_breakage && detected_lateral_breakage) {

				// Repeat 3 times wthin a cell
				cell.getElement().style.backgroundSize = "33.3% 33.3%";

				// checkers
				const stripes_color_hex = "00c";
				const stripes_opacity_fraction = 0.7; // between 0 and 1
				cell.getElement().style.backgroundImage = "url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='8' height='8' viewBox='0 0 8 8'%3E%3Cg fill='%239C92AC' fill-opacity='0.4'%3E%3Cpath fill-rule='evenodd' d='M0 0h4v4H0V0zm4 4h4v4H4V4z'/%3E%3C/g%3E%3C/svg%3E\")";

			} else if (detected_contiguous_breakage) {
				// Repeat 3 times wthin a cell
				cell.getElement().style.backgroundSize = "33.3%";

				// vertical stripes
				const stripes_color_hex = "00c";
				const stripes_opacity_fraction = 0.7; // between 0 and 1
				cell.getElement().style.backgroundImage = "url(\"data:image/svg+xml,%3Csvg width='40' height='1' viewBox='0 0 40 1' xmlns='http://www.w3.org/2000/svg'%3E%3Cpath d='M0 0h20v1H0z' fill='%23" + stripes_color_hex + "' fill-opacity='" + stripes_opacity_fraction + "' fill-rule='evenodd'/%3E%3C/svg%3E\")";

			} else if (detected_lateral_breakage) {

				// Repeat 3 times wthin a cell
				cell.getElement().style.backgroundSize = "auto 33.3%";

				// horizontal stripes
				const stripes_color_hex = "00c";
				const stripes_opacity_fraction = 0.7; // between 0 and 1
				cell.getElement().style.backgroundImage = "url(\"data:image/svg+xml,%3Csvg width='1' height='40' viewBox='0 0 1 40' xmlns='http://www.w3.org/2000/svg'%3E%3Cpath d='M0 20h1v20H0z' fill='%23" + stripes_color_hex + "' fill-opacity='" + stripes_opacity_fraction + "' fill-rule='evenodd'/%3E%3C/svg%3E\")";

			}

			const context_menu_items = get_context_menu_items_for_cell(cell);
			if (context_menu_items.length > 0) {
				cell.getElement().style.cursor = "context-menu";
			}

			if (cell_value != null) {

				const is_success = cell_value.failure_mode["tag"] == "Success";

				const img_path = cell_value.is_flaky ? "yellow-triangle.svg"
					: cell_value.failure_mode["tag"] == "FailedStep" && cell_value.failure_mode["step_failure"]["tag"] == "Timeout" ? "purple-circle.svg"
						: cell_value.failure_mode["tag"] == "FailedStep" && cell_value.failure_mode["step_failure"]["tag"] == "NoMatch" ? "blue-square.svg"
							: cell_value.failure_mode["tag"] == "NoLog" ? "gray-diamond.svg"
								: is_success ? "green-dot.svg" : "red-x.svg";

				const build_id = cell_value["universal_build"]["db_id"];
				const indicator = '<img src="/images/build-status-indicators/' + img_path + '" style="width: 100%; top: 50%;"/>';


				return is_success ? indicator : link(indicator, "/build-details.html?build_id=" + build_id);

			} else {
				return "";
			}
		},
		headerClick: function(e, column) {

			const columnField = column.getField();

			console.log("Column name: " + columnField);

			navigator.clipboard.writeText( columnField )
			  .then(() => {
			    // Success!
			  })
			  .catch(err => {
			    console.log('Something went wrong', err);
			  });
		},
		headerTooltip: "Click to copy to clipboard",
		cellContext: function(e, cell) {

			const dropdown_element = document.getElementById("myDropdown");

			while (dropdown_element.firstChild) {
				dropdown_element.removeChild(dropdown_element.firstChild);
			}

			const context_menu_items = get_context_menu_items_for_cell(cell);
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


function top_page() {
	$('#offset-input').val(0);

	const radios = $('input:radio[name=pagination-mode]');
	radios.filter('[value=start-by-offset]').prop('checked', true);

	update_url_from_form();
}

function next_page() {

	const count = parseInt($('#count-input').val());
	const old_offset = parseInt($('#offset-input').val());

	$('#offset-input').val(old_offset + count);

	update_url_from_form();
}


function get_column_definitions(raw_column_list) {

	var legend_rows = [
		["Symbol", "Meaning"],
	];

	const symbol_pairs = [
		['yellow-triangle.svg', "flaky"],
		['red-x.svg',           "other match"],
		['blue-square.svg',     "no pattern match"],
		['purple-circle.svg',   "timeout"],
		['gray-diamond.svg',    "no log"],
		['green-dot.svg',       "success"],
	];

	for (var x of symbol_pairs) {
		const div_style = {"style": "text-align: right; margin-right: 0.5em;"};
		legend_rows.push([render_tag("div", '<img src="/images/build-status-indicators/' + x[0] + '" style="width: 20px;"/>', div_style), x[1]]);
	}

	const legend = render_table(legend_rows, {"style": "vertical-align: bottom;"}, "Legend", true);

	const commit_column_definition = {
		title: 'Commit<br/><br/><br/><br/>' + legend,
		field: "commit",
		headerVertical: false,
		formatter: function(cell, formatterParams, onRendered) {

			const row_data = cell.getRow().getData();
			const commit_metadata = row_data["commit_metadata"];

			var message_suffix = "";
			if (commit_metadata) {

				const commit_date = new Date(commit_metadata["committer_date"]);
				const day_index = commit_date.getDay();
				const day_ratio = day_index / 7.0;
				const color = tinycolor.fromRatio({h: day_ratio, s: 0.8, l: 0.8});
				const hex_string = color.toHexString();

				cell.getElement().style.borderLeft = "4px solid " + hex_string;

				var pr_link = "";
				const pr_number = row_data["pr_number"];
				if (pr_number !== null) {
					pr_link = render_tag("sup", render_tag("small", link("#" + pr_number, PULL_REQUEST_URL_PREFIX + pr_number)));
				}

				const message_subject = get_commit_subject(commit_metadata["message"]);
				message_suffix = pr_link + ": " + message_subject;
			}

			return sha1_link(cell.getValue()) + message_suffix;
		},
		tooltip: function(cell) {

			const commit_metadata = cell.getRow().getData()["commit_metadata"];
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

	const column_list = [commit_column_definition].concat(generate_column_tree_base(raw_column_list));
	return column_list;
}


function generate_column_tree_base(column_names) {

	const collator = new Intl.Collator(undefined, {numeric: true, sensitivity: 'base'});
	column_names.sort(collator.compare);
	return generate_column_tree_recursive(column_names.map(x => [x, x]), 0);
}


function generate_column_tree_recursive(column_name_suffix_pairs, depth) {

	const colname_pair_by_prefix = {};
	for (var pair of column_name_suffix_pairs) {

		const col = pair[0];
		const suffix = pair[1];

		const chunks = suffix.split("_");
		const prefix = chunks.shift();

		const members = setDefault(colname_pair_by_prefix, prefix, []);
		members.push([col, chunks.join("_")]);
	}

	const column_list = [];

	for (var col_prefix in colname_pair_by_prefix) {

		const grouped_col_pairs = colname_pair_by_prefix[col_prefix];


		if (grouped_col_pairs.length < MIN_HEADER_GROUPING_COLUMNS) {

			for (var col_pair of grouped_col_pairs) {
				column_list.push(define_column(col_pair[0]));
			}

		} else {
			const subcolumn_definitions = generate_column_tree_recursive(grouped_col_pairs, depth + 1);

			// Collapses redundant headers
			var column_group_definition;

			const truncation_threshold = MAX_HORIZONTAL_LETTERS + FITTABLE_LETTERS_PER_COLUMN * Math.max(0, grouped_col_pairs.length - MIN_HEADER_GROUPING_COLUMNS);
			const truncated_column_name = truncate_overlong_horizontal_heading(col_prefix, truncation_threshold);

			if (subcolumn_definitions.length == 1) {

				column_group_definition = {
					title: truncated_column_name + "<br/>" + subcolumn_definitions[0]["title"],
					columns: subcolumn_definitions[0]["columns"],
				}
			} else {
				column_group_definition = {
					title: truncated_column_name,
					columns: subcolumn_definitions,
				}
			}

			column_list.push(column_group_definition);
		}
	}

	return column_list;
}


function truncate_overlong_horizontal_heading(heading, truncation_threshold) {

	if (heading.length > truncation_threshold) {
		return heading.substring(0, truncation_threshold - 1) + "&hellip;"
	} else {
		return heading;
	}
}


function gen_timeline_table(element_id, fetched_data) {

	const column_list = get_column_definitions(fetched_data.columns);


	// global
	breakage_starts_by_job_name = {};
	for (var breakage_span_obj of fetched_data.breakage_spans) {
		for (var affected_job_obj of breakage_span_obj.start.record.payload.affected_jobs) {
			setDefault(breakage_starts_by_job_name, affected_job_obj, []).push(breakage_span_obj);
		}
	}


	const build_failures_by_commit = {};
	for (var failure_obj of fetched_data.failures) {
		const failures_by_job_name = setDefault(build_failures_by_commit, failure_obj.build.vcs_revision, {});
		failures_by_job_name[failure_obj.build.job_name] = failure_obj;
	}

	const table_data = [];
	for (var commit_obj of fetched_data.commits) {
		const row_dict = {};

		const sha1 = commit_obj.record.commit;
		const failures_by_job_name = build_failures_by_commit[sha1] || {};

		row_dict["commit_index"] = commit_obj.db_id;
		row_dict["contiguous_commit_number"] = commit_obj.record.contiguous_index;
		row_dict["commit"] = sha1;
		row_dict["commit_metadata"] = commit_obj.record.metadata;
		row_dict["pr_number"] = commit_obj.record.pr_number;

		for (var job_name in failures_by_job_name) {
			row_dict[job_name] = failures_by_job_name[job_name];
		}

		table_data.push(row_dict);
	}

	// global
	grid_table = new Tabulator("#" + element_id, {
		layout: "fitColumns",
		placeholder: "No Data Set",
		selectable: true,
		columns: column_list,
		data: table_data,
		rowSelectionChanged: function(data, rows) {
			//rows - array of row components for the selected rows in order of selection
			//data - array of data objects for the selected rows in order of selection

			if (rows.length) {
				$("#floating-selection-helper").show();

				const contiguous_indices = data.map(x => x["contiguous_commit_number"]);
				const min_index = Math.min(...contiguous_indices);
				const max_index = Math.max(...contiguous_indices);
				const spanned_row_count = 1 + max_index - min_index;

				$("#floating-selection-helper-header").html("Row span: " + spanned_row_count);

				$("#floating-selection-helper-go-button").click( function() {  mark_failure_span(data); } );
			} else {
				$("#floating-selection-helper").hide();
			}
		},
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

	const offset = $('#offset-input').val();
	const count = $('#count-input').val();

	const sha1 = $('#sha1-input').val() || "nothing";
	const min_commit_index = $('#commit-index-min').val() || 0;
	const max_commit_index = $('#commit-index-max').val() || 0;


	const pagination_mode = document.querySelector('input[name="pagination-mode"]:checked').value;

	var use_sha1_offset = false;
	if (pagination_mode == "start-by-sha1") {
		use_sha1_offset = true;
	}

	var use_commit_index_bounds = false;
	if (pagination_mode == "start-by-commit-index") {
		use_commit_index_bounds = true;
	}

	const parms = {
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


function update_url_from_form() {

	var url_parms;

	const pagination_mode = document.querySelector('input[name="pagination-mode"]:checked').value;

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

	const urlParams = new URLSearchParams(window.location.search);

	const offset = urlParams.get('offset');
	if (offset != null) {
		$('#offset-input').val(offset);
	}

	const count = urlParams.get('count');
	if (count != null) {
		$('#count-input').val(count);
	}


	const commit_index_min = urlParams.get('min_commit_index');
	if (commit_index_min != null) {
		$('#commit-index-min').val(commit_index_min);
	}

	const commit_index_max = urlParams.get('max_commit_index');
	if (commit_index_max != null) {
		$('#commit-index-max').val(commit_index_max);
	}

	const starting_sha1 = urlParams.get('sha1');
	if (starting_sha1 != null) {
		$('#sha1-input').val(starting_sha1);
	}


	const radios = $('input:radio[name=pagination-mode]');

	if (commit_index_min != null && commit_index_max != null) {
		radios.filter('[value=start-by-commit-index]').prop('checked', true);
	} else if (starting_sha1 != null) {
		radios.filter('[value=start-by-sha1]').prop('checked', true);
	} else {
		radios.filter('[value=start-by-offset]').prop('checked', true);
	}
}


function main() {

	$('#is-ongoing-checkbox').change(function() {
		update_subform_visibility(this.checked);
	});


	populate_form_from_url();


	// Close the dropdown menu if the user clicks outside of it
	window.onclick = function(event) {
		if (!event.target.matches('.dropbtn')) {
			const dropdowns = document.getElementsByClassName("dropdown-content");
			for (var i = 0; i < dropdowns.length; i++) {
				const openDropdown = dropdowns[i];
				if (openDropdown.classList.contains('show')) {
					openDropdown.classList.remove('show');
				}
			}
		}
	}

	render_timeline_table();
}

