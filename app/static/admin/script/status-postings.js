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
        series: [{
            name: title,
            data: rows,
            }],
      });
   });
}


function regen_status_notifications_timeline() {

	const hours_count = $("#hours-input").val();
	gen_time_plot('container-status-notifications-by-hour', '/api/status-notifications-by-hour?hours=' + hours_count, "Status notifications", "hour");

	return false;
}


function main() {

	const pr_comment_post_count = 100;
	gen_comment_postings_table("comment-postings-table", "/api/posted-pr-comments?count=" + pr_comment_post_count, "400px");

	regen_status_notifications_timeline();
}

