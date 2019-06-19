
function gather_revision_data(step_id) {
	return {
		step_id: step_id,
		implicated_revision: $('#input-implicated-revision').val(),
		is_broken: $('#is-broken-checkbox').is(":checked"),
		notes: $('#input-notes').val(),
	};
}


function submit_breakage_report(button, build_step_id) {

	$(button).prop("disabled", true);

	console.log("Submitting breakage report...");

	var revision_data = gather_revision_data(build_step_id);

        $.post({
		url: "/api/report-breakage",
		data: revision_data,
		success: function( data ) {

			$(button).prop("disabled", false);

			if (data.success) {
				alert("submitted report with ID: " + data.payload);
			} else {
				alert("Error: " + data.error.message);
			}
		}
        });
}


function gen_builds_table(element_id, data_url, height_string) {

	var column_list = [
		{title: "Line", field: "line_number", width: 100, formatter: function(cell, formatterParams, onRendered) {
			return gen_line_number_cell(cell);
		}},
		{title: "Match (" + render_tag("span", "click to show log context", {"style": "color: #0d0;"}) + ")", field: "line_text", sorter: "string", widthGrow: 8, formatter: function(cell, formatterParams, onRendered) {
				return gen_error_cell_html(cell);
			},
			cellClick: function(e, cell){

				var row_data = cell.getRow().getData();
				var match_id = row_data["match_id"];
				get_log_text(match_id, 5);
			},
		},
		{title: "Pattern", field: "pattern_id", formatter: "link", formatterParams: {urlPrefix: "/pattern-details.html?pattern_id="}, width: 75},
	];

	// Is not the single-entry "best match" table
	if (height_string != null) {
		column_list.push({title: "Specificity", field: "specificity", formatter: "number", width: 100});
	}

	var table = new Tabulator("#" + element_id, {
		height: height_string,
		layout: "fitColumns",
		placeholder: "No Data Set",
		columns: column_list,
		ajaxURL: data_url,
	});
}


function populate_build_info(build_id, parent_data) {

	var data = parent_data["build_info"];

	var local_logview_item_full = link("View full log", "/api/view-log-full?build_id=" + build_id);
	var logview_items = render_list([
		"View log " + link("on CircleCI", "https://circleci.com/gh/pytorch/pytorch/" + build_id),
		local_logview_item_full,
	]);

	var full_commit = data["build"]["vcs_revision"];
	var short_commit = full_commit.substring(0, 7);

	var local_link = link("View " + render_tag("code", short_commit) + " details", "/commit-details.html?sha1=" + full_commit);
	var github_link = link("View " + render_tag("code", short_commit) + " on GitHub", "https://github.com/pytorch/pytorch/commit/" + full_commit);
	var commit_links = render_list([local_link, github_link]);


	var items = [
		["Logs:", logview_items],
		["Revision:", commit_links],
		["Build step:", render_tag("i", data["step_name"])],
		["Branch:", data["build"]["branch"]],
		["Job name:", link(data["build"]["job_name"], "/job-details.html?job=" + data["build"]["job_name"])],
		["Date:", moment(data["build"]["queued_at"]).fromNow()],
	];

 	$("#implicate-self-button").click(function (e) {
		$("#input-implicated-revision").val(full_commit);
	});
	$("#implicate-self-button").prop("disabled", false);

	// TODO Use this for something
	$.getJSON('https://api.github.com/repos/pytorch/pytorch/commits', {"build_id": build_id}, function (data) {
		var commit_list = [];
		for (var value of data) {
			commit_list.push([value["sha1"], value["commit"]["author"]["name"], value["commit"]["message"]]);
		}
		console.log("Commit list: " + commit_list);
	});

        $("#build-info-box").html(render_table_vertical_headers(items));

	populate_breakage_form("submission_button_placeholder", data["step_id"], data);

	if (parent_data["multi_match_count"] > 1) {
		$("#all-matches-section").show();
		gen_builds_table("all-build-matches-table", "/api/build-pattern-matches?build_id=" + build_id, "300px");
	}
}

function get_build_info(build_id) {

	$.getJSON('/api/single-build-info', {"build_id": build_id}, function (data) {
		if (data.success) {
			populate_build_info(build_id, data.payload);
		} else {
		        $("#build-info-box").html(render_tag("span", "Error: " + data.error.message, {"style": "color: red;"}));
		}
	});
}


function gen_pattern_test_link(build_id) {
        $("#pattern-add-link-container").html( link("Add pattern", "/add-pattern.html?build_id=" + build_id) );
}


function rescan_build(button) {

	var build_id = get_build_number();

	$(button).prop("disabled", true);
	$("#scan-throbber").show();

        $.post({
		url: "/api/rescan-build",
		data: {"build": build_id},
		success: function( data ) {

			$(button).prop("disabled", false);
			$("#scan-throbber").hide();

			if (data.success) {
				alert("Success: " + data.payload);
				location.reload();
			} else {
				alert("Error: " + data.error.message);
			}
		},
		error: function( data ) {
			$("#scan-throbber").hide();
		},
        });
}


function get_build_number() {
	var urlParams = new URLSearchParams(window.location.search);
	return urlParams.get('build_id');
}


function main() {

	var build_id = get_build_number();

	get_build_info(build_id);
	gen_builds_table("best-build-matches-table", "/api/best-build-match?build_id=" + build_id, null);

	gen_pattern_test_link(build_id);
}

