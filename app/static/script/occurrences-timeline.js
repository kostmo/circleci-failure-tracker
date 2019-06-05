function setDefault(obj, prop, deflt) {
  return obj.hasOwnProperty(prop) ? obj[prop] : (obj[prop] = deflt);
}

function main() {

   $.getJSON('/api/patterns-timeline', function (data) {

	// keyed by pattern ID
        var series_titles = {};
        var series_points = {};


	for (var point of data.points) {
		var pointlist = setDefault(series_points, point["pattern_id"], []);
		pointlist.push([Date.parse(point["week"]), point["count"]])
	}

	var series_list = [];
	for (var pattern_obj of data.patterns.slice(0, 8)) {
		var pointlist = series_points[pattern_obj["id"]]
		series_list.push({
		    name: pattern_obj["description"],
		    data: pointlist,
		    });
	}


      Highcharts.chart('container-pattern-occurrences-by-week', {

           chart: {
                type: 'line'
            },
            title: {
                text: 'Pattern Occurrences by Week'
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
        series: series_list,
      });
   });
}
