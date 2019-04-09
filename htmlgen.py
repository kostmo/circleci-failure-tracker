import circlefetch


def surround(inner, left, right):
    return left + inner + right


def parens(inner):
    return surround(inner, "(", ")")


def tag(bread, jam, attrs=None):

    opening = [bread]

    if not attrs:
        attrs = {}

    for k, v in sorted(attrs.items()):
        opening.append(k + '="' + str(v) + '"')

    open_tag = surround(" ".join(opening), "<", ">")
    close_tag = surround(bread, "</", ">")
    return surround(jam, open_tag, close_tag)


def make_row(items, header=False):

    tagname = "th" if header else "td"

    inner = "\n".join([tag(tagname, str(val)) for val in items])
    return tag("tr", inner)


def make_link(url, text):
    return tag("a", text, {"href": url})


def format_date(d):
    if not d:
        return "N/A"

    return d.strftime("%I:%M %p on %m/%d/%y")


def gen_circleci_build_url(build_num):
    return "/".join(["https://circleci.com/gh", circlefetch.PROJECT_NAME, circlefetch.REPO_NAME, str(build_num)])


def prep_build_row(row):
    """
    Handles a specific 4-column format
    """

    build_num = row[0]
    git_sha1 = row[1]

    github_url = "/".join(["https://github.com", circlefetch.PROJECT_NAME, circlefetch.REPO_NAME, "commits", git_sha1])

    return [
        make_link(gen_circleci_build_url(build_num), str(build_num)),
        make_link(github_url, tag("code", git_sha1[:10])),
        format_date(row[2]),
        row[3],
    ]


def make_table(headings, rows, truncate=None):

    string = make_row(headings, True)
    row_count = len(rows)

    omitted_count = 0
    if truncate is not None:
        omitted_count = row_count - min(truncate, row_count)
        rows = rows[:truncate]

    string += "\n".join(map(make_row, rows))

    retval = tag("table", tag("tbody", string))
    if omitted_count:
        retval += tag("span", "omitted %d rows" % omitted_count)

    return retval
