
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

	var table = new Tabulator("#" + element_id, {
		height: height_string,
		layout:"fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title:"Line", field: "line_number", width: 100, formatter: function(cell, formatterParams, onRendered) {
				return gen_line_number_cell(cell);
			}},
			{title:"Line text", field: "line_text", sorter: "string", widthGrow: 8, formatter: function(cell, formatterParams, onRendered) {
					return gen_error_cell_html(cell);
				},
				cellClick: function(e, cell){

					if (height_string == null) {
						var row_data = cell.getRow().getData();
						var build_id = row_data["build_number"];
						get_log_text(build_id, 5);
					}
				},
			},
			{title: "Pattern", field: "pattern_id", formatter: "link", formatterParams: {urlPrefix: "/pattern-details.html?pattern_id="}, width: 75},
		],
		ajaxURL: data_url,
	});
}


function populate_build_info(build_id) {

	$.getJSON('/api/single-build-info', {"build_id": build_id}, function (parent_data) {

		var data = parent_data["build_info"];

		var local_logview_item_full = "<a href='/api/view-log-full?build_id=" + build_id + "'>View full log</a>";
		var local_logview_item = "<button onclick='get_log_text(" + build_id + ", 5);'>View error in context</button>";
		var logview_items = render_list([
			"View log <a href='https://circleci.com/gh/pytorch/pytorch/" + build_id + "'>on CircleCI</a>",
			local_logview_item_full,
			local_logview_item,
		]);

		var full_commit = data["build"]["vcs_revision"];
		var short_commit = full_commit.substring(0, 7);

		var local_link = "<a href='/commit-details.html?sha1=" + full_commit + "'>View " + render_tag("code", short_commit) + " details</a>";
		var github_link = "<a href='https://github.com/pytorch/pytorch/commit/" + full_commit + "'>View " + render_tag("code", short_commit) + " on GitHub</a>";
		var commit_links = render_list([local_link, github_link]);


		var items = [
			["Logs:", logview_items],
			["Revision:", commit_links],
			["Build step:", render_tag("i", data["step_name"])],
			["Branch:", data["build"]["branch"]],
			["Job name:", data["build"]["job_name"]],
			["Date:", moment(data["build"]["queued_at"]).fromNow()],
		];

		// TODO Use this for something
		$.getJSON('https://api.github.com/repos/pytorch/pytorch/commits', {"build_id": build_id}, function (data) {
			var commit_list = [];
			$.each(data, function( index, value ) {
				commit_list.push([value["sha1"], value["commit"]["author"]["name"], value["commit"]["message"]]);
			});
			console.log("Commit list: " + commit_list);
		});

	        $("#build-info-box").html(render_table(items));

		populate_breakage_form("submission_button_placeholder", data["step_id"], data);

		if (parent_data["multi_match_count"] > 1) {
			$("#all-matches-section").show();
			gen_builds_table("all-build-matches-table", "/api/build-pattern-matches?build_id=" + build_id, "300px");
		}
	});
}


function gen_pattern_test_link(build_id) {
        $("#pattern-add-link-container").html("<a href='add-pattern.html?build_id=" + build_id + "'>Add pattern</a>");
}


function rescan_build(button) {

	var build_id = get_build_number();

	$(button).prop("disabled", true);

        $.post({
		url: "/api/rescan-build",
		data: {"build": build_id},
		success: function( data ) {

			$(button).prop("disabled", false);

			if (data.success) {
				alert("Authentication success: " + data.payload);
			} else {
				alert("Error: " + data.error.message);
			}
		}
        });
}


function get_build_number() {

	var urlParams = new URLSearchParams(window.location.search);
	return urlParams.get('build_id');
}


function main() {

	var build_id = get_build_number();

	populate_build_info(build_id);
	gen_builds_table("best-build-matches-table", "/api/best-build-match?build_id=" + build_id, null);

	gen_pattern_test_link(build_id);
}

