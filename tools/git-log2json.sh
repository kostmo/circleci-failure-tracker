#!/usr/bin/env bash

# Use this one-liner to produce a JSON literal from the Git log.
# Adapted from: https://gist.github.com/textarcana/1306223


# The use of ^^^^ as a temporary substitution for double-quotes is
# required because some author names can contain double quotes, which break JSON formatting.
# This string should be sufficiently unique such that it does not
# appear in author names or the commit message.
DELIM=^^^^

git log \
    --pretty=format:"{%n  ${DELIM}sha1${DELIM}: ${DELIM}%H${DELIM},%n  ${DELIM}message${DELIM}: ${DELIM}%f${DELIM},%n  ${DELIM}tree_sha1${DELIM}: ${DELIM}%T${DELIM},%n  ${DELIM}author_name${DELIM}: ${DELIM}%an${DELIM},%n  ${DELIM}author_email${DELIM}: ${DELIM}%aE${DELIM},%n  ${DELIM}author_date${DELIM}: ${DELIM}%ai${DELIM},%n  ${DELIM}committer_name${DELIM}: ${DELIM}%cN${DELIM},%n  ${DELIM}committer_email${DELIM}: ${DELIM}%cE${DELIM},%n  ${DELIM}committer_date${DELIM}: ${DELIM}%ci${DELIM}%n}," \
    $@ | \
    perl -pe 'BEGIN{print "["}; END{print "]\n"}' | \
    perl -pe 's/},]/}]/' | \
    sed 's/"/\\"/g' | sed "s/\\$DELIM/\"/g"

