// global
var ranges_by_week = {};

function normalized_build_failure_count_highchart(series_list) {

	Highcharts.chart('container-normalized-build-failures-by-week', {
		chart: {
			type: 'line'
		},
		title: {
			text: 'Build failures by Week'
		},
		colors: ["#8085e9", "#b0b0b0"],
		subtitle: {
			text: 'Showing only full weeks, starting on labeled day'
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
					text: 'build failure rate'
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


function normalized_commit_failure_count_highchart(series_list) {

	Highcharts.chart('container-normalized-commit-failures-by-week', {
		chart: {
			type: 'line'
		},
		title: {
			text: 'Commits with failures by Week'
		},
		colors: ["#f08080", "#b0b0b0"],
		subtitle: {
			text: 'Showing only full weeks, starting on labeled day'
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
					text: 'commit failure rate'
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





function separated_causes_column_highchart(columns, column_chart_series, color_key) {

	var series_list = [];
	for (var key in column_chart_series) {
		var points = column_chart_series[key];

		series_list.push({
			name: key.replace(new RegExp("_", 'g'), " "),
			data: points,
			color: color_key[key],
		});
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

		var undifferentiated_build_failures_series_points = [];
		var undifferentiated_commit_failures_series_points = [];

		var commit_count_series_points = [];

		var column_chart_series = {};
		var column_chart_timestamp_categories = [];

		for (var datum of data["by_week"]) {

			var week_val = Date.parse(datum["week"]);
			var commit_count = datum["commit_count"];

			ranges_by_week[week_val] = datum;

			var weeks_ago_count = moment().diff(week_val, "weeks");
			var weeks_ago_string = weeks_ago_count + " week" + (weeks_ago_count != 1 ? "s" : "") + " ago"
			column_chart_timestamp_categories.push(weeks_ago_string);

			const aggregate_build_counts = datum["aggregate_build_counts"];

			for (var key in aggregate_build_counts) {
				if (!["failure_count"].includes(key)) {

					var normalized_value = 1.0 * aggregate_build_counts[key] / commit_count;

					var pointlist = setDefault(separated_causes_series_points, key, []);
					pointlist.push([week_val, normalized_value])


					var column_chart_pointlist = setDefault(column_chart_series, key, []);
					column_chart_pointlist.push(normalized_value)
				}
			}

			commit_count_series_points.push([week_val, commit_count]);

			undifferentiated_build_failures_series_points.push([week_val, 1.0 * aggregate_build_counts["failure_count"] / commit_count]);

			undifferentiated_commit_failures_series_points.push([week_val, 1.0 * datum["aggregate_commit_counts"]["had_failure"] / commit_count]);
		}

		var separated_causes_series_list = [];
		for (var key in separated_causes_series_points) {
			var pointlist = separated_causes_series_points[key]
			separated_causes_series_list.push({
				name: key.replace(new RegExp("_", 'g'), " "),
				data: pointlist,
				color: data["build_colors"][key],
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


		separated_causes_column_highchart(column_chart_timestamp_categories, column_chart_series, data["build_colors"]);


		var commit_count_series_points = {
			name: "Commit count",
			data: commit_count_series_points,
			yAxis: 0,
			dashStyle: 'shortdot',
		};

	        var undifferentiated_build_failures_series = [
			{
				name: "Build failures per commit",
				data: undifferentiated_build_failures_series_points,
				yAxis: 1,
			},
			commit_count_series_points,
		];


	        var undifferentiated_commit_failures_series = [
			{
				name: "Failed commits",
				data: undifferentiated_commit_failures_series_points,
				yAxis: 1,
			},
			commit_count_series_points,
		];


		normalized_commit_failure_count_highchart(undifferentiated_commit_failures_series);
		normalized_build_failure_count_highchart(undifferentiated_build_failures_series);
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

