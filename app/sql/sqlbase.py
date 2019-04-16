DB_NAME = "loganci"
DB_USERNAME = "logan"
DB_PASSWORD = "logan01"


def get_conn(hostname):
    import psycopg2
    return psycopg2.connect(host=hostname, dbname=DB_NAME, user=DB_USERNAME, password=DB_PASSWORD)
