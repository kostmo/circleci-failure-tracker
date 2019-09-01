var tags_list = [];
var steps_list = [];


function add_tag() {

	const tag_val = $('#pattern-tag-input').val();
	const trimmed_val = tag_val.trim();
	if (trimmed_val) {
		tags_list.push(trimmed_val);

		render_lists();
	}
}

function add_build_step() {

	const step_val = $('#build-step-applicability-input').val();
	steps_list.push(step_val);

	const select = document.getElementById("build-step-applicability-input");

	for (var i=0; i<select.options.length; i++) {
		if (select.options[i].text == step_val) {
			select.options.remove(i);
			break;
		}
	}

	render_lists();
}


function render_list_html(tag_list) {

	const tag_elements = tag_list.map(function(val) {
		const class_list = ["tag", "deletable"];
		if (TAG_CLASSES.has(val)) {
			class_list.push("tag-class-" + val);
		}

		return "<span class='" + class_list.join(" ") + "'>" + val + "</span>";
	});

	return tag_elements.join(" ");
}


function render_lists() {
	$('#tag-list-container').html(render_list_html(tags_list));
	$('#tag-list-container').children().click(function(e) {

		const val = e.target.innerText;
		tags_list = tags_list.filter(item => item !== val);
		render_lists();
	});

	$('#step-list-container').html(render_list_html(steps_list));
	$('#step-list-container').children().click(function(e) {

		const val = e.target.innerText;
		steps_list = steps_list.filter(item => item !== val);

		const select = document.getElementById("build-step-applicability-input");
		select.options[select.options.length] = new Option(val, val);

		render_lists();
	});

	$('#all-steps-applicable-indicator').css('visibility', steps_list.length > 0 ? 'hidden' : 'visible');
}


function submit_pattern() {

	const pattern_data = gather_pattern_data();

        $.post( {
		url: "/api/new-pattern-insert",
		data: pattern_data,
		success: function( data ) {
			if (data.success) {
				alert("submitted pattern with ID: " + data.payload);
					window.location.href = "/";
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


function setup_autocomplete() {

	const select = document.getElementById("build-step-applicability-input");
	$.getJSON('/api/step-list', function (mydata) {
		for (var val of mydata) {
		    select.options[select.options.length] = new Option(val, val);
		}
	});


	$("#pattern-tag-input").autocomplete({
		source: function( request, response ) {
			$.ajax({
				url: "/api/tag-suggest",
				dataType: "json",
				data: {
					term: request.term
				},
				success: function( data ) {
					response( data );
				}
			});
		},
		minLength: 1,
		select: function( event, ui ) {
			console.log( "Selected: " + ui.item.value + " aka " + ui.item.id );
		}
	});
}


function gather_pattern_data() {

	return {
		is_regex: $('#is-regex-checkbox').is(":checked"),
		is_nondeterministic: $('#is-nondeterministic-checkbox').is(":checked"),
		pattern: $('#input-pattern-text').val(),
		description: $('#input-pattern-description').val(),
		specificity: $('#specificity-input').val(),
		tags: tags_list.join(";"),
		applicable_steps: steps_list.join(";"),
		use_lines_from_end: $('#is-using-lines-from-end-checkbox').is(":checked"),
		lines_from_end: $('#input-lines-from-end').val(),
	};
}


function test_pattern() {

	$("#test-match-results-container").html("");

	const pattern_data = gather_pattern_data();
	pattern_data["build_num"] = $('#test-build-id').val();

	$.get( {
		url: "/api/new-pattern-test",
		data: pattern_data,
		success: function( data ) {

			var inner_html = "";

			const matches_list = data.payload.matches;
			if (matches_list.length > 0) {

				const table_rows = [];
				for (var i=0; i<matches_list.length; i++) {
					const match_details = matches_list[i]["match_details"];
					const one_based_line_number = match_details["line_number"] + 1;
					table_rows.push(["Line " + one_based_line_number + ":", match_details["line_text"]]);
				}

				inner_html += render_table(table_rows, {}, null, false);

				const lines_from_end = data.payload.total_line_count - one_based_line_number;
				$('#input-lines-from-end').val( lines_from_end );

			} else {
				inner_html += "<span style='color: red;'>No matches</span>";
			}

			$("#test-match-results-container").html(inner_html);
		}
	});
}


function shipOff(event) {

	const result = event.target.result;

	const parsed_json_from_file = JSON.parse( result );

	$.ajax("/api/patterns-restore", {
	    'data': JSON.stringify(parsed_json_from_file),
	    'type': 'POST',
	    'processData': false,
	    'contentType': 'application/json',
            'success': continueSubmission,
            'error': showUploadFailure,
	});
}


function showUploadFailure(data) {
	alert("FAILED: " + data);
}


function continueSubmission(data) {
	document.getElementById('upload-result-box').value = data;
}


function import_patterns(button_obj) {

	const file = document.getElementById('selectFiles').files[0];
	const reader = new FileReader();
	reader.readAsText(file, 'UTF-8');
	reader.onload = shipOff;

	return false;
}


function prepopulate_build_number() {
	const urlParams = new URLSearchParams(window.location.search);
	const build_id = urlParams.get('build_id');

	if (build_id) {
		$("#test-build-id").val(build_id);
	}
}


function main() {

	$("#is-regex-checkbox").change(function(event) {
		const checkbox = event.target;
		$("#is-nondeterministic-checkbox").prop("disabled", !checkbox.checked);
	});

	setup_autocomplete();
	prepopulate_build_number();
}
