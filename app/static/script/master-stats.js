// global
var ranges_by_week = {};

function timeline_highchart(series_list) {

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
				var commit_id_bounds = ranges_by_week[this.x];
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
		var series_points = {};

		for (var datum of data) {

			var week_val = Date.parse(datum["week"]);
			ranges_by_week[week_val] = datum["commit_id_bound"];

			for (var key in datum) {
				if (key != "week" && key.endsWith("_count") && !["commit_count", "failure_count", "pattern_matched_count"].includes(key)) {

					var pointlist = setDefault(series_points, key, []);
					pointlist.push([week_val, datum[key]])
				}
			}
		}

		var series_list = [];
		for (var key in series_points) {
			var pointlist = series_points[key]
			series_list.push({
			    name: key,
			    data: pointlist,
			    });
		}

		timeline_highchart(series_list);
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

