Dr. CI
===============

You have probably arrived at this document by following a link in a comment left on your Pull Request.

## How does it work?

The Dr. CI application has registered a Webhook with GitHub to receive events upon every *build status notification*
sent to GitHub by CI providers (both third-party like CircleCI and first-party like GitHub Actions).
Receipt of these build status events enqueues a *log scanning job* on Dr. CI for the commit that had been built.
Upon completion of the log scan, Dr. CI posts a comment to the Pull Request summarizing the scan results.

To determine which Pull Request to post this comment to, a Git-hosting webservice [Gadgit](http://gadgit.pytorch.org/) is queried.

## Can I unsubscribe?

Follow [this link](https://dr.pytorch.org/admin/comments-opt-out.html).

## More information on Dr. CI

* [Toplevel README](../..)

### Facebook internal resources:

* [Wiki entry](https://our.internmc.facebook.com/intern/wiki/Dr._CI/)
* [Workplace group](https://fb.workplace.com/groups/488620375234384/)
