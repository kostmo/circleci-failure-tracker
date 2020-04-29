function main() {

	const pr_comment_post_count = 300;
	gen_comment_postings_table("comment-postings-table", "/api/posted-pr-comments?count=" + pr_comment_post_count);
}

