// global
var start_picker;
var end_picker;


function gen_table(element_id, data_url, height_string) {

	const column_list = [
		{title: "Job", field: "name", width: 500,
		},
		{title: "Isolated failures", field: "y", width: 200,
		},
	];

	const table = new Tabulator("#" + element_id, {
		height: height_string,
		layout: "fitColumns",
		placeholder: "No Data Set",
		columns: column_list,
		ajaxURL: data_url,
	});
}


function requery_table() {

	const start_timestamp = start_picker.getDate().toISOString();
	const end_timestamp = end_picker.getDate().toISOString();

	const api_url = "/api/isolated-failures-timespan?start-timestamp=" + start_timestamp + "&end-timestamp=" + end_timestamp;
	gen_table("good-commits-table", api_url, 300);

	return false;
}


// Sets the start date to the previous Sunday, and the
// end date in the future (coming Sunday)
function bounds_this_week() {

	const start_picker_initial_date = getSunday(new Date());
	setSpanDaysForward(start_picker_initial_date, 7)

	requery_table();
}


// Ending at the most recent Sunday, show the 7 preceeding days
function bounds_last_week() {

	const end_picker_initial_date = getSunday(new Date());
	setSpanDaysBackward(end_picker_initial_date, 7);

	requery_table();
}


// Sets the end date in the future (tomorrow)
function bounds_today() {

	const start_picker_initial_date = new Date();

	setSpanDaysForward(start_picker_initial_date, 1)

	requery_table();
}


function bounds_yesterday() {

	const end_picker_initial_date = new Date();
	setSpanDaysBackward(end_picker_initial_date, 1);

	requery_table();
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


function main() {

	// global
	start_picker = new Pikaday({ field: document.getElementById('datepicker-start') });
	end_picker = new Pikaday({ field: document.getElementById('datepicker-end') });


	bounds_this_week();
}

