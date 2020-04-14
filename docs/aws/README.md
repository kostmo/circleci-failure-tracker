# AWS Dependencies

Dr. CI uses the following Amazon Web Servies:

* **Elastic Beanstalk** with Docker for hosting the webapp. See [ELASTIC-BEANSTALK-SERVERS.md](ELASTIC-BEANSTALK-SERVERS.md).
* **Lambda**. See [LAMBDA-FUNCTIONS.md](LAMBDA-FUNCTIONS.md)
* **RDS** to host a Postgres database.
* **SQS** as a work queue for scanning Git commits for CI statuses

