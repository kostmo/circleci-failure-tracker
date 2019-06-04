var tags_list = [];
var steps_list = [];


function add_tag() {

	var tag_val = $('#pattern-tag-input').val();
	tags_list.push(tag_val);

	render_lists();
}

function add_build_step() {

	var step_val = $('#build-step-applicability-input').val();
	steps_list.push(step_val);

	render_lists();
}


function render_list_html(tag_list) {

	var tag_elements = tag_list.map(function(val) {
		var class_list = ["tag"];
		if (TAG_CLASSES.has(val)) {
			class_list.push("tag-class-" + val);
		}

		return "<span class='" + class_list.join(" ") + "'>" + val + "</span>";
	});

	return tag_elements.join(" ");
}


function render_lists() {
	$('#tag-list-container').html(render_list_html(tags_list));
	$('#step-list-container').html(render_list_html(steps_list));
}


function submit_pattern() {

	var pattern_data = gather_pattern_data();

        $.post( {
		url: "/api/new-pattern-insert",
		data: pattern_data,
		success: function( data ) {
			if (data.success) {
				alert("submitted pattern with ID: " + data.payload);
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


function get_random_build(destination_field_id) {

	$.get( {
		url: "/api/random-scannable-build",
		success: function( data ) {
			$("#" + destination_field_id).val(data["build_number"]);
		}
	});
}


function setup_autocomplete() {

    $( "#build-step-applicability-input" ).autocomplete({
      source: function( request, response ) {
        $.ajax( {
          url: "/api/step-suggest",
          dataType: "json",
          data: {
            term: request.term
          },
          success: function( data ) {
            response( data );
          }
        } );
      },
      minLength: 1,
      select: function( event, ui ) {
        console.log( "Selected: " + ui.item.value + " aka " + ui.item.id );
      }
    } );


    $("#pattern-tag-input").autocomplete({
      source: function( request, response ) {
        $.ajax( {
          url: "/api/tag-suggest",
          dataType: "json",
          data: {
            term: request.term
          },
          success: function( data ) {
            response( data );
          }
        } );
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
		tags: tags_list.join(";"),
		applicable_steps: steps_list.join(";"),
		use_lines_from_end: $('#is-using-lines-from-end-checkbox').is(":checked"),
		lines_from_end: $('#input-lines-from-end').val(),
	};
}


function test_pattern() {

	$("#test-match-results-container").html("");

	var pattern_data = gather_pattern_data();
        pattern_data["build_num"] = $('#test-build-id').val();

        $.get( {
          url: "/api/new-pattern-test",
          data: pattern_data,
          success: function( data ) {

		var inner_html = "";

		var matches_list = data.payload.matches;
		if (matches_list.length > 0) {

			var table_rows = [];
			for (var i=0; i<matches_list.length; i++) {
				var match_details = matches_list[i]["match_details"];
				var one_based_line_number = match_details["line_number"] + 1;
				table_rows.push(["Line " + one_based_line_number + ":", match_details["line_text"]]);
			}

			inner_html += render_table(table_rows);

			var lines_from_end = data.payload.total_line_count - one_based_line_number;
			$('#input-lines-from-end').val( lines_from_end );

		} else {
			inner_html += "<span style='color: red;'>No matches</span>";
		}

		$("#test-match-results-container").html(inner_html);
          }
        });
}


function shipOff(event) {

	var result = event.target.result;

	var parsed_json_from_file = JSON.parse( result );

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

	var file = document.getElementById('selectFiles').files[0];
	var reader = new FileReader();
	reader.readAsText(file, 'UTF-8');
	reader.onload = shipOff;

	return false;
}


function prepopulate_build_number() {
	var urlParams = new URLSearchParams(window.location.search);
	var build_id = urlParams.get('build_id');

	if (build_id) {
		$("#test-build-id").val(build_id);
	}
}


function main() {

	$("#is-regex-checkbox").change(function(event) {
		var checkbox = event.target;
		$("#is-nondeterministic-checkbox").prop("disabled", !checkbox.checked);
	});

	setup_autocomplete();
	prepopulate_build_number();
}
