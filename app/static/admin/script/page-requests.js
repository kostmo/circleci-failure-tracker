// global
const ranges_by_week = {};


function make_timeline_highchart(container_element, series_list, stacking_type, label_prefix) {

	Highcharts.chart(container_element, {
		chart: {
			type: 'area'
		},
		title: {
			text: 'Failure Modes by Week (per commit)',
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
				text: label_prefix + ' per commit'
			},
			min: 0
		},
		tooltip: {
			useHTML: true,
			style: {
				pointerEvents: 'auto'
			},
			pointFormatter: function() {
				const commit_id_bounds = ranges_by_week[this.x]["commit_id_bound"];

				const unnormalized_val = ranges_by_week[this.x][this.series.name];

				const parms_string = $.param({
					"min_commit_index": commit_id_bounds["min_bound"],
					"max_commit_index": commit_id_bounds["max_bound"],
				});

				const link_url = "/master-timeline.html?" + parms_string;
				const content = this.y.toFixed(2) + " (" + unnormalized_val + ")<br/>" + link("(details)", link_url);
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
			    stacking: stacking_type,
			},
		},
		credits: {
			enabled: false
		},
		series: series_list,
	});
}


function gen_series(data) {

	const series_dict = {};

	for (var datum of data) {

		const week_val = Date.parse(datum["timestamp"]);
		const key = datum["record"]["page_url"];
		const count = datum["record"]["count"];

		const pointlist = setDefault(series_dict, key, []);
		pointlist.push([week_val, count])
	}


	const series_list = [];
	for (var key in series_dict) {
		const pointlist = series_dict[key]

		series_list.push({
			name: key,
			data: pointlist,
		});
	}

	return series_list;
}


function render_master_stats(weeks_count) {

	getJsonWithThrobber("#scan-throbber", "/api/page-views-by-week", {"weeks": weeks_count}, function (data) {

		const series_list = gen_series(data);

		make_timeline_highchart('container-stacked-timeline-by-week', series_list, 'normal', 'counts');
	});
}


function main() {

//	const weeks_count = $('#weeks-count-input').val();
	const weeks_count = 10;
	render_master_stats(weeks_count);
}

