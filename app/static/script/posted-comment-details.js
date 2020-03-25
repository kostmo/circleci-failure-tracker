function get_pr_number() {
	const urlParams = new URLSearchParams(window.location.search);
	return urlParams.get('pr');
}


function main() {

	const pr_number = get_pr_number();

	gen_comment_postings_table("comment-postings-table", "/api/posted-comments-for-pr?pr=" + pr_number);
}

