

function populate_commit_info(commit_sha1, data) {

	const github_link = link("View on GitHub", "https://github.com/pytorch/pytorch/commits/" + commit_sha1);
	const counts_obj = data["payload"]["content"]["counts"];

	const items = [
		["Commit ancestry:", github_link],
		["Failed build count:", counts_obj["failed_build_count"]],
		["Total matched build count:", counts_obj["total_matched_build_count"]],
		["Flaky build count:", counts_obj["flaky_build_count"]],
		["Other matched build count:", counts_obj["other_matched_build_count"]],
		["Unmatched count (excluding timeouts/no logs):", counts_obj["unmatched_count"]],
		["Timeout count:", counts_obj["timeout_count"]],
		["No logs count:", counts_obj["idiopathic_count"]],
		["Known broken count:", counts_obj["known_broken_count"]],
	];

	const stats_table = render_table_vertical_headers(items);

	var analysis_text;

	if (counts_obj["failed_build_count"] == 0) {
		analysis_text = render_tag("span", "No CircleCI builds failed.", {"style": "color: green;"});

	} else if (counts_obj["failed_build_count"] == counts_obj["flaky_build_count"]) {
		analysis_text = render_tag("span", "All of the CircleCI build failures were due to intermittent causes. Consider rerunning them.", {"style": "color: green;"});

	} else if (counts_obj["failed_build_count"] == counts_obj["total_matched_build_count"]) {
		analysis_text = "All of the CircleCI build failures matched with predefined patterns.";
	} else {
		analysis_text = "Some of the build failure causes weren't determined. Please investigate below.";
	}

	const analysis_summary_items = [
		render_tag("h3", "Analysis"),
		render_tag("p", analysis_text),
	];

	const info_box_items = [
		stats_table,
		analysis_summary_items.join(""),
	];

	const info_box_content = info_box_items.join("");

        $("#commit-info-box").html(info_box_content);
}


function fetch_commit_info(commit_sha1) {

	$.getJSON('/api/commit-info', {"sha1": commit_sha1}, function (data) {

		if (data.success) {
			populate_commit_info(commit_sha1, data);
		} else {
		        $("#commit-info-box").html(render_tag("span", "Error: " + data.error.message, {"style": "color: red;"}));
		}
	});
}


function gen_builds_table(element_id, data_url) {

	const table = new Tabulator("#" + element_id, {
		height:"300px",
		layout:"fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title: "Line", field: "match.line_number", width: 100,
				formatter: function(cell, formatterParams, onRendered) {
					return gen_line_number_cell_with_count(cell, cell.getRow().getData()["match"]["line_count"]);
				},
			},
			{title: "Job", width: 300, field: "build.build_record.job_name",
				tooltip: "Click to copy",
				cellClick: function(e, cell) {

					navigator.clipboard.writeText( cell.getValue() )
					  .then(() => {
					    // Success!
					  })
					  .catch(err => {
					    console.log('Something went wrong', err);
					  });
				},
			},
			{title: "Step", width: 250, field: "match.build_step"},
			{title: "Build", field: "build.build_record.build_id",
				formatter: function(cell, formatterParams, onRendered) {

					const row_data = cell.getRow().getData();

					const provider_build_number = row_data["build"]["build_record"]["build_id"];
					const universal_build_number = row_data["build"]["universal_build"]["db_id"];

					return render_build_link_cell(universal_build_number, row_data["provider"]["record"]["icon_url"], provider_build_number);
				},
				tooltip: function(cell) {
					const row_data = cell.getRow().getData();
					return row_data["provider"]["record"]["label"];
				},
				width: 90,
			},
			{title: "Match (" + render_tag("span", "click to show log context", {"style": "color: #0d0;"}) + ")",
				field: "match.line_text",
				sorter: "string",
				widthGrow: 8,
				formatter: function(cell, formatterParams, onRendered) {
					const row_data = cell.getRow().getData();

					const start_idx = row_data["match"]["span_start"];
					const end_idx = row_data["match"]["span_end"];
							
					return gen_error_cell_html_parameterized(cell, start_idx, end_idx);
				},
				cellClick: function(e, cell) {
					const row_data = cell.getRow().getData();
					const build_id = row_data["build"]["build_id"];
					get_log_text(row_data["match"]["match_id"], STANDARD_LOG_CONTEXT_LINECOUNT);
				},
			},
			{title: "Pattern", field: "match.pattern_id", formatter: "link",
				formatterParams: {urlPrefix: "/pattern-details.html?pattern_id="},
				width: 75,
			},
		],
		ajaxURL: data_url,
		ajaxResponse: function(url, params, response) {

			console.log("Loaded build records in " + response.payload.timing.toFixed(1) + " seconds.");
			return response.payload.content;
		},
	});
}


function render_build_link_cell(universal_build_number, icon_url, provider_build_number) {

	const url = "/build-details.html?build_id=" + universal_build_number;
	return '<img src="' + icon_url + '?s=16" style="vertical-align: middle"/> ' + render_tag("span", link(provider_build_number, url), {"style": "vertical-align: middle"})
}


function gen_unmatched_build_list(api_endpoint, div_id) {

	$.getJSON(api_endpoint, function (data) {

		if (data.length == 0) {
			return;
		}

		$("#" + div_id + "-parent").show();


		const table = new Tabulator("#" + div_id, {
			height:"200px",
			layout:"fitColumns",
			placeholder:"No Data Set",
			columns:[
				{title:"Build number", field: "build",

					formatter: function(cell, formatterParams, onRendered) {

						const row_data = cell.getRow().getData();

						const provider_build_number = cell.getValue();
						const universal_build_number = row_data["universal_build_number"];

						return render_build_link_cell(universal_build_number, row_data["provider_icon_url"], provider_build_number);
					},
					tooltip: function(cell) {
						const row_data = cell.getRow().getData();
						return row_data["provider_label"];
					},
					width: 90,
				},
				{title:"Step", field:"step_name", width: 200},
				{title:"Job", field:"job_name", width: 200},
				{title:"Time", field:"queued_at", width: 150,
					formatter: function(cell, formatterParams, onRendered) {
						const val = cell.getValue();
						return moment(val).fromNow();
					},
				},
				{title:"Branch", field:"branch", width: 150},
			],
			data: data,
		});
	});
}


function rescan_commit(button) {

	const commit_sha1 = get_scrubbed_sha1();
	$(button).prop("disabled", true);
	$("#scan-throbber").show();

        $.post({
		url: "/api/rescan-commit",
		data: {
			"sha1": commit_sha1,
			"login_redirect_path": get_url_path_for_redirect(),
		},
		success: function( data ) {

			$(button).prop("disabled", false);
			$("#scan-throbber").hide();

			handle_submission_response(data);
		},
		error: function( data ) {
			$("#scan-throbber").hide();
			alert("Server error!");
		},
        });
}


function handle_submission_response(data) {

	if (data.success) {
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


// XXX hack around HUD URL-generation logic which appends "/console"
function get_scrubbed_sha1() {

	const urlParams = new URLSearchParams(window.location.search);
	const commit_sha1 = urlParams.get('sha1');
	const found_slash_index = commit_sha1.indexOf("/");
	if (found_slash_index >= 0) {
		commit_sha1 = commit_sha1.substring(0, found_slash_index);
	}

	return commit_sha1;
}


function main() {

	const commit_sha1 = get_scrubbed_sha1();

	fetch_commit_info(commit_sha1);
	gen_builds_table("builds-table", "/api/commit-builds?sha1=" + commit_sha1);

	gen_unmatched_build_list("/api/unmatched-builds-for-commit?sha1=" + commit_sha1, "container-unattributed-failures");
	gen_unmatched_build_list("/api/idiopathic-failed-builds-for-commit?sha1=" + commit_sha1, "container-idiopathic-failures");
	gen_unmatched_build_list("/api/timed-out-builds-for-commit?sha1=" + commit_sha1, "container-timeout-failures");
}
