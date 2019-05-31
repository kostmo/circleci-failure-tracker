
function gather_revision_data(full_sha1) {
	return {
            revision: full_sha1,
            implicated_revision: $('#input-implicated-revision').val(),
            is_broken: $('#is-broken-checkbox').is(":checked"),
            notes: $('#input-notes').val(),
          };
}


function submit_breakage_report(button, full_sha1) {

	$(button).prop("disabled", true);

	console.log("Submitting report for revision: " + full_sha1);

	var revision_data = gather_revision_data(full_sha1);

        $.post( {
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
        } );
}


function gen_builds_table(element_id, data_url, height_string) {

	var table = new Tabulator("#" + element_id, {
	    height: height_string,
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title:"Line", field: "line_number", width: 100, formatter: function(cell, formatterParams, onRendered) {
			return (cell.getValue() + 1) + " / " + cell.getRow().getData()["line_count"];
		  }},
		{title:"Line text", field: "line_text", sorter: "string", widthGrow: 8, formatter: function(cell, formatterParams, onRendered) {
			return gen_error_cell_html(cell);
		  },
			cellClick: function(e, cell){
			    $("#error-display").html(gen_error_cell_html(cell));
		    },
	        },
	    ],
            ajaxURL: data_url,
	});
}


function get_log_text(build_id) {
	$.getJSON('/api/view-log', {"build_id": build_id}, function (data) {
		console.log(data);

		if (data.success) {
			var w = window.open('', '', 'resizeable,scrollbars');
			w.document.write("<html><head><title>Log output</title></head><body><code style='white-space: pre'>" + data.payload + "</code></body></html>");
			w.document.close();
		} else {
			var proceed = confirm("Need to login first...");
			if (proceed) {
				window.location.href = data.error.details.login_url;
			}
		}
	});
}


function populate_build_info(build_id) {

	$.getJSON('/api/single-build-info', {"build_id": build_id}, function (data) {

		var local_logview_item = "<button onclick='get_log_text(" + build_id + ");'>View log</button>";
//		var local_logview_item = "<a href='/api/view-log?build_id=" + build_id + "'>Download log</a>";
		var logview_items = "<ul><li>View log <a href='https://circleci.com/gh/pytorch/pytorch/" + build_id + "'>on CircleCI</a></li><li>" + local_logview_item + "</li></ul>";

		var full_commit = data["build"]["vcs_revision"];
		var short_commit = full_commit.substring(0, 7);

		var github_link = "<a href='https://github.com/pytorch/pytorch/commit/" + full_commit + "'>View <code>" + short_commit + "</code> on GitHub</a>";
		var local_link = "<a href='/commit-details.html?sha1=" + full_commit + "'>View <code>" + short_commit + "</code> builds</a>";
		var commit_links = "<ul><li>" + github_link + "</li><li>" + local_link + "</li><ul>"

		var html = "<dl>"
		html += render_pair("CircleCI page:", logview_items);
		html += render_pair("Build step:", "<i>" + data["step_name"] + "</i>");
		html += render_pair("Branch:", data["build"]["branch"]);
		html += render_pair("Job name:", data["build"]["job_name"]);
		html += render_pair("Date:", data["build"]["queued_at"]);
		html += render_pair("Revision:", commit_links);
		html += "</dl>";

		$.getJSON('https://api.github.com/repos/pytorch/pytorch/commits', {"build_id": build_id}, function (data) {
			var commit_list = [];
			$.each(data, function( index, value ) {
				commit_list.push([value["sha1"], value["commit"]["author"]["name"], value["commit"]["message"]]);
			});
			console.log("Commit list: " + commit_list);
		});

	        $("#build-info-box").html(html);

		populate_breakage_form("submission_button_placeholder", data["build"]["vcs_revision"], data);

	});
}

function gen_pattern_test_link(build_id) {

        $("#pattern-add-link-container").html("<a href='add-pattern.html?build_id=" + build_id + "'>Add pattern</a>");
	
}


function main() {

	var urlParams = new URLSearchParams(window.location.search);
	var build_id = urlParams.get('build_id');

	populate_build_info(build_id);
	gen_builds_table("all-build-matches-table", "/api/build-pattern-matches?build_id=" + build_id, "300px");
	gen_builds_table("best-build-matches-table", "/api/best-build-match?build_id=" + build_id, null);

	gen_pattern_test_link(build_id);
}

