function gen_time_plot(container_id, api_path) {

   $.getJSON(api_path, function (data) {

        var rows = [];

        $.each(data.rows, function( index, value ) {

                rows.push([Date.parse(value[0]), value[1]]);
        });

      Highcharts.chart(container_id, {

           chart: {
                type: 'line'
            },
            title: {
                text: 'Status postings by day'
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
                    text: 'Post count'
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
        series: [{
            name: "Status postings",
            data: rows,
            }],
      });
   });
}

function gen_builds_table(element_id, data_url) {

	var table = new Tabulator("#" + element_id, {
	    height:"400px",
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title: "Revision", field: "sha1", width: 100, formatter: function(cell, formatterParams, onRendered) {
			return '<code><a href="/commit-details.html?sha1=' + cell.getValue() + '">' + cell.getValue().substring(0, 7) + '</a></code>';
		  }},
		{title: "Message", field: "description", sorter: "string"},
		{title: "Time", field: "created_at", formatter: function(cell, formatterParams, onRendered) {
			return moment(cell.getValue()).fromNow();
		    }
		},
	    ],
            ajaxURL: data_url,
	});
}


function main() {

	gen_builds_table("status-postings-table", "/api/posted-statuses");
	gen_time_plot('container-status-postings-by-day', '/api/status-postings-by-day');
}

