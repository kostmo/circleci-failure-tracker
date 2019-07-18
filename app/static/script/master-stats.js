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
				var content = this.y + "<br/>" + link("(details)", "/master-timeline.html?min_commit_index=" + commit_id_bounds["min_bound"] + "&max_commit_index=" + commit_id_bounds["max_bound"]);
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


function separated_causes_timeline_highchart(series_list) {

	Highcharts.chart('container-pattern-occurrences-by-week', {
		chart: {
			type: 'area'
		},
		title: {
			text: 'Failure Modes by Week'
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
				text: 'count'
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
				var content = this.y + "<br/>" + link("(details)", "/master-timeline.html?min_commit_index=" + commit_id_bounds["min_bound"] + "&max_commit_index=" + commit_id_bounds["max_bound"]);
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

	$.getJSON('/api/master-weekly-failure-stats', {"weeks": weeks_count}, function (data) {

		// keyed by pattern ID
		var separated_causes_series_points = {};

		var general_failures_series = [];
		var commit_count_series = [];

		for (var datum of data) {

			var week_val = Date.parse(datum["week"]);
			ranges_by_week[week_val] = datum;

			for (var key in datum) {
				if (key != "week" && key.endsWith("_count") && !["commit_count", "failure_count", "pattern_matched_count"].includes(key)) {

					var pointlist = setDefault(separated_causes_series_points, key, []);
					pointlist.push([week_val, datum[key]])
				}

			}

			commit_count_series.push([week_val, datum["commit_count"]]);
			general_failures_series.push([week_val, 1.0 * datum["failure_count"] / datum["commit_count"]]);
		}

		var separated_causes_series_list = [];
		for (var key in separated_causes_series_points) {
			var pointlist = separated_causes_series_points[key]
			separated_causes_series_list.push({
				name: key,
				data: pointlist,
			});
		}

		separated_causes_timeline_highchart(separated_causes_series_list);




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
	render();
}

