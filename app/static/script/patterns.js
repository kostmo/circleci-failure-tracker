function setup_autocomplete() {

	$( "#branch-name-input" ).autocomplete({
		source: function( request, response ) {
			$.ajax({
				url: "/api/branch-suggest",
				dataType: "json",
				data: {
					term: request.term
				},
				success: function( data ) {
					response( data );
				}
			});
		},
		minLength: 1,
		select: function( event, ui ) {
			console.log( "Selected: " + ui.item.value + " aka " + ui.item.id );
			requery_patterns();
		}
	});
}


function requery_patterns() {

	const branch_name = $("#branch-name-input").val();

	const checked_radio_value = $("input[name='filter-mode']:checked").val();

	const used_presumed_stable_branches = checked_radio_value == "stable-branches";
	const branches_list = checked_radio_value == "single-branch" ? [branch_name] : [];

	gen_patterns_table(null, used_presumed_stable_branches, branches_list);
}


function make_tag_histogram(container_id) {

	$.getJSON("/api/tags", function (data) {

		const category_list = [];
		const build_count_list = [];
		for (var tag_obj of data) {
			category_list.push(tag_obj.tag);
			build_count_list.push(tag_obj.build_count);
		}

		Highcharts.chart(container_id, {
			chart: {
				type: 'column'
			},
			title: {
				text: 'Tagged builds'
			},
			xAxis: {
				categories: category_list,
				crosshair: true,
			},
			yAxis: {
				min: 0,
				title: {
					text: 'count'
				}
			},
			tooltip: {
				headerFormat: '<span style="font-size:10px">{point.key}</span><table>',
				pointFormat: '<tr><td style="color:{series.color};padding:0">{series.name}: </td>' +
				'<td style="padding:0"><b>{point.y:.1f} mm</b></td></tr>',
				footerFormat: '</table>',
				shared: true,
				useHTML: true,
			},
			plotOptions: {
				column: {
					pointPadding: 0.2,
					borderWidth: 0,
				}
			},
			series: [{
				name: 'Build count',
				data: build_count_list,
				},
			],
		});
	});
}


function main() {

	$('input[type=radio][name=filter-mode]').change(function() {
		console.log("Radio changed. Requerying...");
		requery_patterns();
	});

	setup_autocomplete();
	requery_patterns();

	make_tag_histogram("container-tag-builds");
}
