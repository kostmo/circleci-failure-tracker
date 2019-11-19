// global
var start_picker;
var end_picker;

function gen_table(element_id, data_payload, height_string) {

	const column_list = [
		{title: "Job", field: "job", width: 500,
		},
		{title: "Isolated failures", field: "isolated_failure_count", width: 200,
		},
		{title: "Recognized as flaky", field: "recognized_flaky_count", width: 200,
		},

		{title: "Matched by patterns", field: "matched_count", width: 200,
		},
		{title: "Timed out", field: "timeout_count", width: 200,
		},
		{title: "Timeline Span", field: "min_commit_index",
			formatter: function(cell, formatterParams, onRendered) {
				const row_data = cell.getRow().getData();

				const extra_args = {
					"max_columns_suppress_successful": 35,
					"should_suppress_scheduled_builds": true,
					"should_suppress_fully_successful_columns": true,
				};

				return link("View", gen_master_timeline_commit_bounds_url(row_data.min_commit_index, row_data.max_commit_index, extra_args));
			},
		},
	];

	const table = new Tabulator("#" + element_id, {
		height: height_string,
		layout: "fitColumns",
		placeholder: "No Data Set",
		columns: column_list,
		data: data_payload,
		rowClick:function(e, row) {
			const job_name = row.getData()["job"];
			console.log("Clicked job:", job_name);
		},
	});
}


function requery_table(query_args_dict) {

	$("#full-span-grid-link-placeholder").hide();
	getJsonWithThrobber("#throbber", "/api/isolated-failures-timespan", query_args_dict, function (data) {

		if (data.success) {

			if (data.payload.length) {
				const min_commit_idx = Math.min(...data.payload.map(x => x.min_commit_index));
				const max_commit_idx = Math.max(...data.payload.map(x => x.max_commit_index));

				console.log("commit index span:", min_commit_idx, max_commit_idx);



				const min_commit_number = Math.min(...data.payload.map(x => x.min_commit_number));
				const max_commit_number = Math.max(...data.payload.map(x => x.max_commit_number));

				console.log("commit number span:", min_commit_number, max_commit_number);
				const commit_count = max_commit_number - min_commit_number + 1;



				const link_url = gen_master_timeline_commit_bounds_url(min_commit_idx, max_commit_idx);
				$("#full-span-grid-link-placeholder").html(link("View " + commit_count + "-commit span on master timeline", link_url));

				$("#full-span-grid-link-placeholder").show();
			}

			gen_table("good-commits-table", data.payload, 300);

		} else {

			// TODO consolidate this error handling with "handle_submission_response()" from "html-utils.js"
			if (data.error.details.authentication_failed) {
				window.location.href = data.error.details.login_url;
			}
		}
	});

	return false;
}


// Sets the start date to the previous Sunday, and the
// end date in the future (coming Sunday)
function bounds_this_week() {

	const start_picker_initial_date = getSunday(new Date());
	setSpanDaysForward(start_picker_initial_date, 7)

	const start_timestamp = start_picker.getDate().toISOString();
	const query_args_dict = {
		"start-timestamp": start_timestamp,
	};

	requery_table(query_args_dict);
}


// Ending at the most recent Sunday, show the 7 preceeding days
function bounds_last_week() {

	const end_picker_initial_date = getSunday(new Date());
	setSpanDaysBackward(end_picker_initial_date, 7);



	const start_timestamp = start_picker.getDate().toISOString();
	const end_timestamp = end_picker.getDate().toISOString();

	const query_args_dict = {
		"start-timestamp": start_timestamp,
		"end-timestamp": end_timestamp,
	};

	requery_table(query_args_dict);
}


// Sets the end date in the future (tomorrow)
function bounds_today() {

	const start_picker_initial_date = new Date();

	setSpanDaysForward(start_picker_initial_date, 1)


	const start_timestamp = start_picker.getDate().toISOString();

	const query_args_dict = {
		"start-timestamp": start_timestamp,
	};


	requery_table(query_args_dict);
}


function bounds_yesterday() {

	const end_picker_initial_date = new Date();
	setSpanDaysBackward(end_picker_initial_date, 1);


	const start_timestamp = start_picker.getDate().toISOString();
	const end_timestamp = end_picker.getDate().toISOString();

	const query_args_dict = {
		"start-timestamp": start_timestamp,
		"end-timestamp": end_timestamp,
	};

	requery_table(query_args_dict);
}


// See https://stackoverflow.com/a/4156516/105137
function getSunday(base_date) {

	var day = base_date.getDay(),
	diff = base_date.getDate() - day;
	return new Date(base_date.setDate(diff));
}


function setSpanDaysForward(start_picker_initial_date, day_count) {

	start_picker.setDate(start_picker_initial_date);


	const end_picker_initial_date = new Date();
	end_picker_initial_date.setDate(start_picker_initial_date.getDate() + day_count);

	end_picker.setDate(end_picker_initial_date);
}


function setSpanDaysBackward(end_picker_initial_date, day_count) {

	end_picker.setDate(end_picker_initial_date);

	const start_picker_initial_date = new Date();
	start_picker_initial_date.setDate(end_picker_initial_date.getDate() - day_count);

	start_picker.setDate(start_picker_initial_date);
}


function go_trailing_days() {


	const trailing_days_count = $("#trailing-days-input").val();
	console.log("Thing:", trailing_days_count);

	const end_picker_initial_date = new Date();
	setSpanDaysBackward(end_picker_initial_date, trailing_days_count);


	const start_timestamp = start_picker.getDate().toISOString();

	const query_args_dict = {
		"start-timestamp": start_timestamp,
	};

	requery_table(query_args_dict);
}


function go_calendar_span() {

	const start_timestamp = start_picker.getDate().toISOString();
	const end_timestamp = end_picker.getDate().toISOString();

	const query_args_dict = {
		"start-timestamp": start_timestamp,
		"end-timestamp": end_timestamp,
	};

	requery_table(query_args_dict);
}


function main() {

	// global
	start_picker = new Pikaday({ field: document.getElementById('datepicker-start') });
	end_picker = new Pikaday({ field: document.getElementById('datepicker-end') });


	bounds_this_week();
}

