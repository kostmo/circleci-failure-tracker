// global
var ranges_by_week = {};

function normalized_failure_count_highchart(series_list) {

	Highcharts.chart('container-normalized-failures-by-week', {
		chart: {
			type: 'line'
		},
		title: {
			text: 'Failures by Week'
		},
		colors: ["#f08080", "#b0b0b0"],
		subtitle: {
			text: 'Showing only full weeks, starting on labeled day'
		},
		annotations: [{
			labelOptions: {
				backgroundColor: 'rgba(255,255,255,0.5)',
				verticalAlign: 'top',
				y: 15
			},
			labels: [{
				point: {
					xAxis: 0,
					yAxis: 0,
					x: new Date('2019-06-10T00:00:00Z'),
					y: 400,
				},
				text: 'Start data collection',
			}],
		}],
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
		yAxis: [
			{
				title: {
					text: 'commits',
				},
				opposite: true,
				min: 0,
			},
			{
				title: {
					text: 'failures'
				},
				min: 0,
			},
		],
		tooltip: {
			useHTML: true,
			style: {
				pointerEvents: 'auto'
			},
			pointFormatter: function() {
				var commit_id_bounds = ranges_by_week[this.x]["commit_id_bound"];
				var link_url = "/master-timeline.html?min_commit_index=" + commit_id_bounds["min_bound"] + "&max_commit_index=" + commit_id_bounds["max_bound"];

				var y_val = this.series.name == "Commit count" ? this.y : this.y.toFixed(2);
				var content = y_val + "<br/>" + link("(details)", link_url);
				return content;
			},
		},
		plotOptions: {
			line: {
				marker: {
					enabled: true
				}
			},
		},
		credits: {
			enabled: false
		},
		series: series_list,
	});
}

function separated_causes_column_highchart(columns, column_chart_series) {

	var series_list = [];
	for (var key in column_chart_series) {
		var points = column_chart_series[key];

		series_list.push({name: key.replace(new RegExp("_", 'g'), " "), data: points});
	}


	Highcharts.chart('container-column-separated-occurrences-by-week', {
		chart: {
			type: 'column'
		},
		title: {
			text: 'Failure Modes by Week (per commit)'
		},
		subtitle: {
			text: 'Showing only full weeks, starting on labeled day'
		},
		xAxis: {
			categories: columns,
			crosshair: true
		},
		yAxis: {
			min: 0,
			title: {
				text: 'count per commit'
			}
		},
		tooltip: {
			headerFormat: '<span style="font-size:10px">{point.key}</span><table>',
			pointFormat: '<tr><td style="color:{series.color};padding:0">{series.name}: </td>' +
				'<td style="padding:0"><b>{point.y:.2f}</b></td></tr>',
			footerFormat: '</table>',
			shared: true,
			useHTML: true
		},
		plotOptions: {
			column: {
				pointPadding: 0.2,
				borderWidth: 0
			}
		},
		series: series_list,
	});
}


function separated_causes_timeline_highchart(series_list) {

	Highcharts.chart('container-stacked-timeline-separated-occurrences-by-week', {
		chart: {
			type: 'area'
		},
		title: {
			text: 'Failure Modes by Week (per commit)'
		},
		subtitle: {
			text: 'Showing only full weeks, starting on labeled day'
		},
		annotations: [{
			labelOptions: {
				backgroundColor: 'rgba(255,255,255,0.5)',
				verticalAlign: 'top',
				y: 15
			},
			labels: [{
				point: {
					xAxis: 0,
					yAxis: 0,
					x: new Date('2019-06-10T00:00:00Z'),
					y: 400,
				},
				text: 'Start data collection',
			}],
		}],
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
				text: 'count per commit'
			},
			min: 0
		},
		tooltip: {
			useHTML: true,
			style: {
				pointerEvents: 'auto'
			},
			pointFormatter: function() {
				var commit_id_bounds = ranges_by_week[this.x]["commit_id_bound"];

				var unnormalized_val = ranges_by_week[this.x][this.series.name];

				var link_url = "/master-timeline.html?min_commit_index=" + commit_id_bounds["min_bound"] + "&max_commit_index=" + commit_id_bounds["max_bound"];
				var content = this.y.toFixed(2) + " (" + unnormalized_val + ")<br/>" + link("(details)", link_url);
				return content;
			},
		},
		plotOptions: {
			line: {
				marker: {
					enabled: true
				}
			},
			area: {
			    stacking: 'normal',
			},
		},
		credits: {
			enabled: false
		},
		series: series_list,
	});
}


function render() {

	var weeks_count = $('#weeks-count-input').val();

	$("#scan-throbber").show();
	$.getJSON('/api/master-weekly-failure-stats', {"weeks": weeks_count}, function (data) {

		$("#scan-throbber").hide();

		var separated_causes_series_points = {};

		var general_failures_series = [];
		var commit_count_series = [];

		var column_chart_series = {};
		var column_chart_timestamp_categories = [];

		for (var datum of data) {

			var week_val = Date.parse(datum["week"]);
			var commit_count = datum["commit_count"];

			ranges_by_week[week_val] = datum;

			var weeks_ago_count = moment().diff(week_val, "weeks");
			var weeks_ago_string = weeks_ago_count + " week" + (weeks_ago_count != 1 ? "s" : "") + " ago"
			column_chart_timestamp_categories.push(weeks_ago_string);

			for (var key in datum) {
				if (key != "week" && key.endsWith("_count") && !["commit_count", "failure_count"].includes(key)) {

					var normalized_value = 1.0 * datum[key] / commit_count;

					var pointlist = setDefault(separated_causes_series_points, key, []);
					pointlist.push([week_val, normalized_value])


					var column_chart_pointlist = setDefault(column_chart_series, key, []);
					column_chart_pointlist.push(normalized_value)
				}
			}

			commit_count_series.push([week_val, commit_count]);
			general_failures_series.push([week_val, 1.0 * datum["failure_count"] / commit_count]);
		}

		var separated_causes_series_list = [];
		for (var key in separated_causes_series_points) {
			var pointlist = separated_causes_series_points[key]
			separated_causes_series_list.push({
				name: key.replace(new RegExp("_", 'g'), " "),
				data: pointlist,
			});
		}


		// Sort series by volume
		separated_causes_series_list.sort(function(a, b) {

			const add = (a, b) => a + b;
			var get_area = z => z.data.map(x => x[1]).reduce(add);

			// in descending order
			return get_area(b) - get_area(a);
		});

		separated_causes_timeline_highchart(separated_causes_series_list);


		separated_causes_column_highchart(column_chart_timestamp_categories, column_chart_series);



	        var my_series = [
			{
				name: "Failures per commit",
				data: general_failures_series,
				yAxis: 1,
			},
			{
				name: "Commit count",
				data: commit_count_series,
				yAxis: 0,
				dashStyle: 'shortdot',
			},
		];

		normalized_failure_count_highchart(my_series);
	});
}

function populate_form_from_url() {

	var urlParams = new URLSearchParams(window.location.search);

	var weeks_count = urlParams.get('weeks_count');
	if (weeks_count != null) {
		$('#weeks-count-input').val(weeks_count);
	}
}


function main() {
	populate_form_from_url();
	render();
}

