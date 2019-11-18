// global
var start_picker;
var end_picker;


function gen_table(element_id, data_url, height_string) {

	const column_list = [
		{title: "Job", field: "name", width: 400,
		},
		{title: "Count", field: "y", width: 125,
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


function setOneWeekSpan(end_picker_initial_date) {

	end_picker.setDate(end_picker_initial_date);


	const start_picker_initial_date = new Date();
	start_picker_initial_date.setDate(end_picker_initial_date.getDate() - 7);

	start_picker.setDate(start_picker_initial_date);
}


function main() {

	// global
	start_picker = new Pikaday({ field: document.getElementById('datepicker-start') });
	end_picker = new Pikaday({ field: document.getElementById('datepicker-end') });


	const end_picker_initial_date = new Date();
	setOneWeekSpan(end_picker_initial_date);


	requery_table();

}

