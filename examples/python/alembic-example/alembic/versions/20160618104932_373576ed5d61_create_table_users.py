from alembic import op
# import sqlalchemy as sa
from sqlalchemy import text

"""create table users

Revision ID: 373576ed5d61
Revises:
Create Date: 2016-06-18 10:49:32.717195

"""

# revision identifiers, used by Alembic.
revision = '373576ed5d61'
down_revision = None
branch_labels = None
depends_on = None


def upgrade():
    print("{} upgrade: starting transaction".format(__file__))
    conn = op.get_bind()
    transaction = conn.begin()

    try:
        print("{} upgrade: creating scheama".format(__file__))
        conn.execute(text("""CREATE SCHEMA alembic_example;"""))
        print("{} upgrade: creating table".format(__file__))
        conn.execute(text("""CREATE TABLE alembic_example.users (
            id          bigserial,
            created_at  timestamp with time zone default now() NOT NULL,
            updated_at  timestamp with time zone default now() NOT NULL,
            first_name  varchar(255) NOT NULL,
            last_name   varchar(255) NOT NULL,
            email       varchar(255) NOT NULL
        );"""))
        print("{} upgrade: creating index".format(__file__))
        conn.execute(text("""
                          CREATE UNIQUE INDEX idx_users_email
                              ON alembic_example.users (email)
                          """))
        print("{} upgrade: committing".format(__file__))
        transaction.commit()
        print("{} upgrade: done!".format(__file__))
    except:
        print("{} upgrade: error, rolling back!".format(__file__))
        transaction.rollback()
        raise


def downgrade():
    raise Exception("Error: no down is supported for this migration")
