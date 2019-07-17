
function timeline_highchart(series_list) {

	Highcharts.chart('container-pattern-occurrences-by-week', {

		chart: {
			type: 'line'
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
				return this.y;
			},
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
		series: series_list,
	});

}


function breakdown() {

	$.getJSON('/api/master-weekly-failure-stats', {"weeks": 6}, function (data) {

		// keyed by pattern ID
		var series_points = {};

		for (var datum of data) {

			var week_val = Date.parse(datum["week"]);

			for (var key in datum) {
				if (key != "week" && key.endsWith("_count")) {

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

function main() {
	breakdown();
}

