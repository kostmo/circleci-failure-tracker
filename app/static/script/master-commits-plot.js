

function breakages_gantt_highchart(api_data) {


	const series_data = [{
		    start: Date.UTC(2019, 6, 1),
		    end: Date.UTC(2019, 7, 1),
		    name: 'Fake Thing'
		}];

	for (var datum of api_data) {

		const entry = {
			start: Date.parse(datum.start),
			end: Date.parse(datum.end),
			name: 'Breakage',
			prNumber: datum.pr,
		}


		if (datum.foreshadowed_by_pr_failures) {
			entry["color"] = "red";
		}

		series_data.push(entry);
	}

	Highcharts.ganttChart('breakage-spans-gantt-container', {

		title: {
			text: 'Master breakages'
		},
		tooltip: {
			pointFormat: '<span>PR <a href="' + PULL_REQUEST_URL_PREFIX + '{point.prNumber}">#{point.prNumber}</a></span><br/><span>From: {point.start:%e. %b}</span><br/><span>To: {point.end:%e. %b}</span>'
		},
	    yAxis: {
		uniqueNames: true
	    },

	    navigator: {
		enabled: true,
		liveRedraw: true,
		series: {
		    type: 'gantt',
		    pointPlacement: 0.5,
		    pointPadding: 0.25
		},
		yAxis: {
		    min: 0,
		    max: 3,
		    reversed: true,
		    categories: []
		}
	    },
		credits: {
			enabled: false
		},
	    scrollbar: {
		enabled: true
	    },
	    rangeSelector: {
		enabled: true,
		selected: 0
	    },

	    series: [{
		name: 'Master viability',
		data: series_data,
	    }]
	});
}


function commits_timeline_highchart(series_list) {

	Highcharts.chart('container-stacked-timeline-separated-occurrences-by-week', {
		chart: {
			type: 'area', // TODO use "step"
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



	$("#scan-throbber").show();
	$.getJSON('/api/master-commits-granular',  function (data) {

		$("#scan-throbber").hide();

		breakages_gantt_highchart(data);


//		commits_timeline_highchart();

	});
}


function main() {
	render();
}

