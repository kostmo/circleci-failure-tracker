function gen_throughput_plot(container_id, api_path, title, time_unit) {

	$.getJSON(api_path, function (data) {

		const enqueued_rows = [];
		const completed_rows = [];
		const incomplete_rows = [];
		for (var value of data.rows) {
			const date_val = Date.parse(value.first_inserted_hour);

			enqueued_rows.push([date_val, value.enqueued_count]);
			completed_rows.push([date_val, value.completed_count]);
			incomplete_rows.push([date_val, value.enqueued_count - value.completed_count]);
		}

		const series_list = [
			{
				type: 'area',
				marker: {
					enabled: false,
				},
				fillColor: '#f88',
				name: "Incomplete",
				data: incomplete_rows,
			},
			{
				type: 'area',
				marker: {
					enabled: false,
				},
				fillOpacity: 0,
				name: "Complete",
				data: completed_rows,
				showInLegend: false,
			},
			{
				color: '#000',
				type: 'line',
				name: "Enqueued",
				data: enqueued_rows,
			},
			{
				color: '#7e7',
				type: 'line',
				name: "Completed",
				data: completed_rows,
			},
		];

		Highcharts.chart(container_id, {

			chart: {
				type: 'line'
			},
			time: {
				useUTC: false
			},
			title: {
				text: title + ' by ' + time_unit
			},
			xAxis: {
				type: 'datetime',
				dateTimeLabelFormats: {
					day: '%e. %b',
				},
				title: {
					text: 'Date'
				}
			},
			yAxis: {
				title: {
					text: title
				},
				min: 0,
				hour: '%H:%M',
			},
			tooltip: {
				headerFormat: '<b>{series.name}</b><br>',
				pointFormat: '{point.x:%e. %b}: {point.y}'
			},
			plotOptions: {
				area: {
					stacking: 'normal',
				},
				line: {
					marker: {
						enabled: true,
					},
				},
			},
			credits: {
				enabled: false,
			},
			series: series_list,
		
		});
	});
}

function gen_time_plot(container_id, api_path, title, time_unit) {

	$.getJSON(api_path, function (data) {

	const rows = [];
	for (var value of data.rows) {
		rows.push([Date.parse(value[0]), value[1]]);
	}

	Highcharts.chart(container_id, {

		chart: {
			type: 'line'
		},
		time: {
			useUTC: false
		},
		title: {
			text: title + ' by ' + time_unit
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
				text: title
			},
			min: 0
		},
		tooltip: {
			headerFormat: '<b>{series.name}</b><br>',
			pointFormat: '{point.x:%e. %b}: {point.y}'
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
		series: [
				{
					name: title,
					data: rows,
				}
			],
		});
	});
}


function regen_status_notifications_timeline() {

	const hours_count = $("#hours-input").val();
	gen_time_plot('container-status-notifications-by-hour', '/api/status-notifications-by-hour?hours=' + hours_count, "Status notifications", "hour");

	gen_throughput_plot('container-throughput-by-hour', '/api/throughput-by-hour?hours=' + hours_count, "Throughput", "hour");

	return false;
}


function main() {

	const pr_comment_post_count = 100;
	gen_comment_postings_table("comment-postings-table", "/api/posted-pr-comments?count=" + pr_comment_post_count, "400px");

	regen_status_notifications_timeline();
}

