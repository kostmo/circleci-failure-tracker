// global
var breakage_starts_by_job_name = {};


function get_timeline_data(offset, count) {

	$.getJSON('/api/master-timeline', {"offset": offset, "count": count}, function (mydata) {
		gen_timeline_table("master-timeline-table", mydata);
	});
}


function get_context_menu_items_for_cell(cell) {

	var job_name = cell.getColumn().getField();
	var job_names_delimited = [job_name].join(";"); // TODO

	var commit_sha1 = cell.getRow().getData()["commit"];


	var context_menu_items = [];

	var cell_value = cell.getValue();
	if (cell_value != null) {
		var node = document.createElement("span");
		var textnode = document.createTextNode("Mark failure start");
		node.appendChild(textnode);

		node.addEventListener("click", function () {mark_failure_cause(commit_sha1, job_names_delimited);});

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



function mark_failure_cause(commit_sha1, jobs_list_delimited) {

	console.log("Submitting breakage report...");

	var description = prompt("Enter description of breakage:", "no comment");

	if (description != null) {
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
	}
}


function gen_timeline_table(element_id, fetched_data) {

	var column_list = [];

	var commit_column_definition = {
		title: "Commit",
		field: "commit",
		headerVertical: false,
		formatter: function(cell, formatterParams, onRendered) {
			return sha1_link(cell.getValue());
		},
		minWidth: 90,
		width: 90,
		resizable: true,
		headerSort: false,
		frozen: true,
	};

	column_list.push(commit_column_definition);

	var filtered_column_names = fetched_data.columns.filter(x => !x.startsWith("binary"));
	filtered_column_names.sort();

	for (var col of filtered_column_names) {
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

				var cell_value = cell.getValue();
				if (cell_value != null) {
					return cell_value["failure_mode"]["contents"]["line_text"];
				} else {
					return false;
				}
			},
			formatter: function(cell, formatterParams, onRendered) {

				var open_breakages = get_open_breakages(cell);
				if (open_breakages.length > 0) {
					cell.getElement().style.backgroundColor = "#fcc8";
				}

				var context_menu_items = get_context_menu_items_for_cell(cell);
				if (context_menu_items.length > 0) {
					cell.getElement().style.cursor = "context-menu";
				}

				var cell_value = cell.getValue();
				if (cell_value != null) {
					var img_path = cell_value.is_flaky ? "yellow-x.svg" : "red-x.svg";
					return '<img src="/images/' + img_path + '" style="width: 100%; top: 50%;"/>';
				} else {
					return "";
				}
			},
			cellClick: function(e, cell) {

				var cell_value = cell.getValue()
				console.log("cell: " + cell_value + "; commit: " + cell.getRow().getData()["commit"]);

				if (cell_value != null) {
					var build_id = cell_value["build"]["build_id"];

					var should_view = confirm("View build " + build_id + "?");
					if (should_view) {
						window.location.href = "/build-details.html?build_id=" + build_id;
					}
				}
			},
			cellContext: function(e, cell) {

				var cell_value = cell.getValue();


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
		column_list.push(col_dict);
	}



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

		var sha1 = commit_obj.record;
		var failures_by_job_name = build_failures_by_commit[sha1] || {};

		row_dict["commit_index"] = commit_obj.db_id;
		row_dict["commit"] = sha1;

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
}


function main() {

	init_fields();


	// Close the dropdown menu if the user clicks outside of it
	window.onclick = function(event) {
		if (!event.target.matches('.dropbtn')) {
			var dropdowns = document.getElementsByClassName("dropdown-content");
			var i;
			for (i = 0; i < dropdowns.length; i++) {
				var openDropdown = dropdowns[i];
				if (openDropdown.classList.contains('show')) {
					openDropdown.classList.remove('show');
				}
			}
		}
	}

	render_table();
}

