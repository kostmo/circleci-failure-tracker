function dashboard_main() {


	const weeks_count = $('#weeks-count-input').val();


	$.getJSON("/api/downstream-impact-weekly", {"weeks": weeks_count}, function (data) {

		const split_series_list = generate_downstream_impact_series_list(data, true);
		plot_downstream_impact_by_week("container-stacked-percent-downstream-impact-by-week", split_series_list, "percent", "percent");

	});


	render(weeks_count);

}

