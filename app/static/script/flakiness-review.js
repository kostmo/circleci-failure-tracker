// global
var start_picker;
var end_picker;


var global_unmatched_failure_details_table = null;

function gen_failure_details_table(element_id, data_payload, height_string) {

	const table = new Tabulator("#" + element_id, {
		height: height_string,
		layout: "fitColumns",
		placeholder: "No Data Set",
		columns: gen_builds_table_columns(),
		data: data_payload,
	});
}


function load_pattern_failure_details(pattern_id, commit_id_min, commit_id_max) {

	$("#selected-details-row-title-container").html("pattern " + pattern_id);
	
	const query_args_dict = {
		"pattern": pattern_id,
		"commit-id-min": commit_id_min,
		"commit-id-max": commit_id_max,
	};

	$("#failure-details-section").show();

	$("#failure-details-table").hide();
	getJsonWithThrobber("#throbber-details-table", "/api/master-pattern-failures-in-timespan", query_args_dict, function (data) {

		if (data.success) {

			$("#failure-details-table").show();
			gen_failure_details_table("failure-details-table", data.payload, 300);

		} else {

			alert("error: " + data.error);
		}
	});
}


function rescan_selected_builds(button) {

	$(button).prop("disabled", true);

	const selected_data = global_unmatched_failure_details_table.getSelectedData();

	console.log("Will scan", selected_data.length, "rows...");


	const build_ids = selected_data.map(x => x["universal_build_id"]);

	console.log("Build IDs:", build_ids);

	const throbber_id = "#mini-throbber";
	$(throbber_id).show();

	$.ajax("/api/rescan-multiple-builds?login_redirect_path=" + get_url_path_for_redirect(), {
		'data': JSON.stringify(build_ids),
		'type': 'POST',
		'processData': false,
		'contentType': 'application/json',
		'success': function(data) {
			$(throbber_id).hide();
			$(button).prop("disabled", false);
			alert("Finished scanning.");
		},
		'error': function(data) {
			$(throbber_id).hide();
			$(button).prop("disabled", false);
			alert("FAILED: " + data);
		},
	});
}


function make_timeline_chart(element_id, rows) {


      Highcharts.chart(element_id, {

           chart: {
                type: 'line'
            },
            title: {
                text: 'Isolated failures by day'
            },
            xAxis: {
                type: 'datetime',
                dateTimeLabelFormats: { // don't display the dummy year
                    month: '%e. %b',
                    year: '%b'
                },
                title: {
                    text: 'Date'
                }
            },
            yAxis: {
                title: {
                    text: 'Isolated failures fraction'
                },
                min: 0
            },
            tooltip: {
                headerFormat: '<b>{series.name}</b><br>',
                pointFormat: '{point.x:%e. %b}: {point.y:.2f} m'
            },

            plotOptions: {
                line: {
                    marker: {
                        enabled: true
                    }
                }
            },

        credits: {
            enabled: false
        },
        series: [{
            name: "Isolated failures fraction",
            data: rows,
            }],
      });
}


function make_coarse_cause_pie(container_id, data) {

	const filtered_data = data.filter(d => d.y > 0);

	Highcharts.chart(container_id, {
		chart: {
			type: 'pie',

			plotBackgroundColor: null,
			plotBorderWidth: null,
			plotShadow: false,

			margin: [0, 0, 0, 0],
			spacingTop: 0,
			spacingBottom: 0,
			spacingLeft: 0,
			spacingRight: 0,
		},
		title: {
//			text: 'Failure causes',
			text: null,
		},
		plotOptions: {
			pie: {
				allowPointSelect: true,
				cursor: 'pointer',
				dataLabels: {
					enabled: true,
					format: '<b>{point.name}</b>: {point.percentage:.1f} %',
					style: {
						color: (Highcharts.theme && Highcharts.theme.contrastTextColor) || 'black'
					}
				}
			}
		},
		credits: {
			enabled: false
		},
		series: [{
			name: "Count",
			colorByPoint: true,
			data: filtered_data,
		}],
	});

}


function load_coarse_cause_bins(query_args) {

	getJsonWithThrobber("#throbber-coarse-cause-bins", "/api/isolated-failures-timespan-coarse-bins", query_args, function (data) {

		if (data.success) {
			make_coarse_cause_pie("container-coarse-cause-piechart", data.payload);
		} else {
			alert("error: " + data.error);
		}
	});
}


function load_day_highchart() {

	getJsonWithThrobber("#throbber-isolated-failures-timeline-by-day", "/api/isolated-master-failures-by-day", {"age-days": 60}, function (data) {

		if (data.success) {

			const rows = data.payload.map(v => [Date.parse(v[0]), v[1]]);
			make_timeline_chart("container-isolated-failures-timeline-by-day", rows);

		} else {
			alert("error: " + data.error);
		}
	});
}


function load_job_failure_details(job_name, commit_id_min, commit_id_max) {

	$("#selected-details-row-title-container").html("job " + job_name);
	
	const query_args_dict = {
		"job": job_name,
		"commit-id-min": commit_id_min,
		"commit-id-max": commit_id_max,
	};

	$("#failure-details-section").show();

	$("#failure-details-table").hide();
	getJsonWithThrobber("#throbber-details-table", "/api/master-job-failures-in-timespan", query_args_dict, function (data) {

		if (data.success) {

			$("#failure-details-table").show();
			gen_failure_details_table("failure-details-table", data.payload, 300)

		} else {
			alert("error: " + data.error);
		}
	});
}


function gen_patterns_table(element_id, data_payload, height_string) {

	const column_list = [
		{title: "Pattern expression", field: "expression", 
			formatter: function(cell, formatterParams, onRendered) {
				if (cell.getValue() != null) {
					return cell.getValue();
				} else {
					return "<span style='font-style: italic; color: #7cb5ec;'>&lt;no pattern match&gt;</span>";
				}
			},
		},
		{title: "Network", field: "is_network", formatter: "tickCross", width: 100,
			align: "center",
			formatterParams: {
				allowEmpty:true,
				crossElement: false,
			}
		},
		{title: "Isolated failures", field: "isolated_failure_count", width: 150,
		},
		{title: "Recognized as flaky", field: "recognized_flaky_count", width: 200,
		},
		{title: "Timeline Span", field: "min_commit_index",
			formatter: function(cell, formatterParams, onRendered) {
				const row_data = cell.getRow().getData();

				const extra_args = {
					"max_columns_suppress_successful": 35,
					"should_suppress_scheduled_builds": true,
					"should_suppress_fully_successful_columns": true,
					"highlight_job": row_data.job,
				};

				const commit_count = row_data["max_commit_number"] - row_data["min_commit_number"] + 1;

				return link(pluralize(commit_count, "commit"), gen_master_timeline_commit_bounds_url(row_data.min_commit_index, row_data.max_commit_index, extra_args));
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
			const row_data = row.getData()
			const pattern_id = row_data["pattern_id"];

			console.log("Clicked pattern:", pattern_id);

			const commit_id_min = row_data["min_commit_index"];
			const commit_id_max = row_data["max_commit_index"];

			if (pattern_id != null) {
				$("#rescan-failures-button").hide();
				load_pattern_failure_details(pattern_id, commit_id_min, commit_id_max);
			} else {

				const query_args_dict = {
					"commit-id-min": commit_id_min,
					"commit-id-max": commit_id_max,
				};

				gen_unmatched_failures_table(query_args_dict);
			}
		},
	});
}


function gen_unmatched_failures_table(query_args_dict) {

	getJsonWithThrobber("#throbber-details-table", "/api/isolated-unmatched-failed-builds-master-commit-range", query_args_dict, function (data) {

		if (data.success) {

			$("#selected-details-row-title-container").html("unmatched logs");

			global_unmatched_failure_details_table = new Tabulator("#failure-details-table", {
				height:"200px",
				layout:"fitColumns",
				placeholder:"No Data Set",
				selectable: true,
				columns: get_unmatched_build_columns(),
				data: data.payload,
			});

			$("#rescan-failures-button").show();
			$("#failure-details-section").show();
		}
	});
}


function gen_jobs_table(element_id, data_payload, height_string) {

	const column_list = [
		{title: "Job", field: "job", width: 500,
		},
		{title: "Isolated failures", field: "isolated_failure_count", width: 150,
		},
		{title: "Recognized as flaky", field: "recognized_flaky_count", width: 200,
		},
		{title: "Matched by patterns", field: "matched_count", width: 200,
		},
		{title: "Timed out", field: "timeout_count", width: 150,
		},
		{title: "Timeline Span", field: "min_commit_index",
			formatter: function(cell, formatterParams, onRendered) {
				const row_data = cell.getRow().getData();

				const extra_args = {
					"max_columns_suppress_successful": 35,
					"should_suppress_scheduled_builds": true,
					"should_suppress_fully_successful_columns": true,
					"highlight_job": row_data.job,
				};

				const commit_count = row_data["max_commit_number"] - row_data["min_commit_number"] + 1;

				return link(pluralize(commit_count, "commit"), gen_master_timeline_commit_bounds_url(row_data.min_commit_index, row_data.max_commit_index, extra_args));
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
			const row_data = row.getData()
			const job_name = row_data["job"];

			console.log("Clicked job:", job_name);

			const commit_id_min = row_data["min_commit_index"];
			const commit_id_max = row_data["max_commit_index"];

			load_job_failure_details(job_name, commit_id_min, commit_id_max);
		},
	});
}


function requery_by_pattern_table(query_args_dict) {

//	$("#full-span-grid-link-placeholder-by-job").hide();
	getJsonWithThrobber("#throbber-by-pattern", "/api/isolated-failures-timespan-by-pattern", query_args_dict, function (data) {

		if (data.success) {

			if (data.payload.length) {
				const min_commit_idx = Math.min(...data.payload.map(x => x.min_commit_index));
				const max_commit_idx = Math.max(...data.payload.map(x => x.max_commit_index));

				console.log("commit index span:", min_commit_idx, max_commit_idx);


				const min_commit_number = Math.min(...data.payload.map(x => x.min_commit_number));
				const max_commit_number = Math.max(...data.payload.map(x => x.max_commit_number));

				console.log("commit number span:", min_commit_number, max_commit_number);
				const commit_count = max_commit_number - min_commit_number + 1;
			}

			gen_patterns_table("isolated-failures-by-pattern-table", data.payload, 200);

		} else {
			alert("error: " + data.error);
		}
	});
}


function requery_by_job_table(query_args_dict) {

	const full_span_grid_link_element = $("#full-span-grid-link-placeholder-by-job");

	full_span_grid_link_element.hide();
	getJsonWithThrobber("#throbber-by-job", "/api/isolated-failures-timespan-by-job", query_args_dict, function (data) {

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
				full_span_grid_link_element.html(link("View " + commit_count + "-commit span on master timeline", link_url));

				full_span_grid_link_element.show();
			}

			gen_jobs_table("isolated-failures-by-job-table", data.payload, 200);

		} else {
			alert("error: " + data.error);
		}
	});
}


function requery_tables(query_args_dict) {

	const grouping_mode = document.querySelector('input[name="grouping-mode"]:checked').value;

	update_url_from_form(grouping_mode, query_args_dict);

	if (grouping_mode == "by-job") {

		$("#by-pattern-container").hide();
		$("#by-job-container").show();

		requery_by_job_table(query_args_dict);
	} else {
		$("#by-job-container").hide();
		$("#by-pattern-container").show();

		requery_by_pattern_table(query_args_dict);
	}

	$("#failure-details-section").hide();

	load_coarse_cause_bins(query_args_dict);

	load_day_highchart();

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

	requery_tables(query_args_dict);
}



function bounds_go_back_one_week() {


	const end_picker_initial_date = start_picker.getDate();


	console.log("Setting", 7, "days backwards from end date:", end_picker_initial_date);


	setSpanDaysBackward(end_picker_initial_date, 7);

	const start_timestamp = start_picker.getDate().toISOString();
	const end_timestamp = end_picker.getDate().toISOString();

	const query_args_dict = {
		"start-timestamp": start_timestamp,
		"end-timestamp": end_timestamp,
	};

	requery_tables(query_args_dict);
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

	requery_tables(query_args_dict);
}


// Sets the end date in the future (tomorrow)
function bounds_today() {

	const start_picker_initial_date = new Date();

	setSpanDaysForward(start_picker_initial_date, 1)


	const start_timestamp = start_picker.getDate().toISOString();

	const query_args_dict = {
		"start-timestamp": start_timestamp,
	};

	requery_tables(query_args_dict);
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

	requery_tables(query_args_dict);
}


// See https://stackoverflow.com/a/4156516/105137
function getSunday(base_date) {

	var day = base_date.getDay(),
	diff = base_date.getDate() - day;
	return new Date(base_date.setDate(diff));
}


function setSpanDaysForward(start_picker_initial_date, day_count) {

	start_picker.setDate(start_picker_initial_date);

	const end_picker_initial_date = moment(start_picker_initial_date).add(day_count, 'days').toDate();
	end_picker.setDate(end_picker_initial_date);
}


function setSpanDaysBackward(end_picker_initial_date, day_count) {

	end_picker.setDate(end_picker_initial_date);

	const start_picker_initial_date = moment(end_picker_initial_date).subtract(day_count, 'days').toDate();
	start_picker.setDate(start_picker_initial_date);
}


function go_trailing_days() {

	const trailing_days_count = $("#trailing-days-input").val();

	const end_picker_initial_date = new Date();
	setSpanDaysBackward(end_picker_initial_date, trailing_days_count);


	const start_timestamp = start_picker.getDate().toISOString();

	const query_args_dict = {
		"start-timestamp": start_timestamp,
	};

	requery_tables(query_args_dict);
}


function go_calendar_span() {

	const start_timestamp = start_picker.getDate().toISOString();
	const end_timestamp = end_picker.getDate().toISOString();

	const query_args_dict = {
		"start-timestamp": start_timestamp,
		"end-timestamp": end_timestamp,
	};

	requery_tables(query_args_dict);
}



function populate_form_from_url() {

	// TODO

	const url_parms = new URLSearchParams(window.location.search);

	populate_nonnull_field_from_url(url_parms, 'start-timestamp', "datepicker-start");
	populate_nonnull_field_from_url(url_parms, 'end-timestamp', "datepicker-end");
}



function update_url_from_form(grouping_mode, query_args_dict) {

	const extra_dict_elements = {
		"grouping-mode": grouping_mode,
	};

	const url_parms_dict = Object.assign({}, query_args_dict, extra_dict_elements);

	history.pushState(url_parms_dict, "Flakiness review...", "?" + $.param( url_parms_dict ));
}


function main() {

	// global
	start_picker = new Pikaday({ field: document.getElementById('datepicker-start') });
	end_picker = new Pikaday({ field: document.getElementById('datepicker-end') });

	const radios = $('input:radio[name=grouping-mode]')
	radios.on('change', function(e) {
		go_calendar_span();
	});

	bounds_this_week();
}

