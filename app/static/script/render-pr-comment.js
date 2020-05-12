function get_comment_revision_id() {
	const urlParams = new URLSearchParams(window.location.search);
	return urlParams.get('comment_revision');
}


function main() {

	const comment_revision = get_comment_revision_id();

	getJsonWithThrobber(
		"#throbber",
		"/api/posted-comment-revision-body",
		{"comment_revision": comment_revision},
		function (response) {

			get_rendered_markdown_html(response.payload, function( rendered_html ) {

				document.write(rendered_html);
			});
		}
	);
}
