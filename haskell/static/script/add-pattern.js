


function submit_pattern() {

	var pattern_data = gather_pattern_data();

        $.post( {
          url: "/api/new-pattern-insert",
          data: pattern_data,
          success: function( data ) {
		alert("submitted pattern: " + JSON.stringify(data));
          }
        } );
}


function get_random_build(destination_field_id) {

        $.get( {
          url: "/api/random-scannable-build",
          success: function( data ) {
            $("#" + destination_field_id).val(data["build_number"]);

          }
        } );
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


    $( "#pattern-tag-input" ).autocomplete({
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
    } );
}



function gather_pattern_data() {
	return {
            is_regex: $('#is-regex-checkbox').is(":checked"),
            is_nondeterministic: $('#is-nondeterministic-checkbox').is(":checked"),
            pattern: $('#input-pattern-text').val(),
            description: $('#input-pattern-description').val(),
            tags: $('#pattern-tag-input').val(),
            applicable_steps: $('#build-step-applicability-input').val(),
          };
}




function test_pattern() {

	$("#test-match-results-container").html( "" );

	var pattern_data = gather_pattern_data();
        pattern_data["build_num"] = $('#test-build-id').val();

        $.get( {
          url: "/api/new-pattern-test",
          data: pattern_data,
          success: function( data ) {

		var inner_html = "";

		if (data.length > 0) {
			inner_html += "<table><tbody>";
			for (var i=0; i<data.length; i++) {
				var match_details = data[i]["match_details"];
				inner_html += "<tr><td>Line " + match_details["line_number"] + ":</td><td>" + match_details["line_text"] + "</td></tr>";
			}

			inner_html += "</tbody></table>";
		} else {
			inner_html += "<span style='color: red;'>No matches</span>";
		}

		$("#test-match-results-container").html( inner_html );
          }
        } );
}





function main() {
	setup_autocomplete();
}
