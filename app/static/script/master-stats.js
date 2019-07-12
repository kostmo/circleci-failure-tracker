
function timeline_highchart(series_list) {

      Highcharts.chart('container-pattern-occurrences-by-week', {

           chart: {
                type: 'line'
            },
            title: {
                text: 'Failure Modes by Week'
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
			var pattern_id = parseInt(this.series.name.slice(0, this.series.name.indexOf(":")));
	                var headerFormat = '<a href="/pattern-details.html?pattern_id=' + pattern_id + '">' + this.series.name + '</a></b><br/>';
	                var pointFormat = this.y;

			return headerFormat + pointFormat;
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

	/*
	$.getJSON('/api/master-build-stats', function (data) {
		console.log(JSON.stringify(data));
	});
	*/

	$.getJSON('/api/master-weekly-failure-stats', {"weeks": 6}, function (data) {

		// keyed by pattern ID
		var series_points = {};

		for (var datum of data) {

			var week_val = Date.parse(datum["week"]);

			for (var key in datum) {
				if (key != "week") {

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

