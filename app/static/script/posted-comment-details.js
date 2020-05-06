function get_pr_number() {
	const urlParams = new URLSearchParams(window.location.search);
	return urlParams.get('pr');
}


function main() {

	const pr_number = get_pr_number();

	gen_comment_postings_table("latest-comment-posting-table", "/api/latest-posted-comment-for-pr?pr=" + pr_number);
	gen_comment_postings_table("all-comment-postings-table", "/api/all-posted-comments-for-pr?pr=" + pr_number);
}

