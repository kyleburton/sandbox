from alembic import op
from sqlalchemy import text

"""create table events

Revision ID: 9779c33e9ba1
Revises: 373576ed5d61
Create Date: 2016-06-18 11:05:47.654438

"""

# revision identifiers, used by Alembic.
revision = '9779c33e9ba1'
down_revision = '373576ed5d61'
branch_labels = None
depends_on = None


def upgrade():
    print("{} upgrade: starting transaction".format(__file__))
    conn = op.get_bind()
    transaction = conn.begin()

    try:
        print("{} upgrade: creating table".format(__file__))
        conn.execute(text("""CREATE TABLE alembic_example.events (
            id          bigserial,
            created_at  timestamp with time zone default now() NOT NULL,
            updated_at  timestamp with time zone default now() NOT NULL,
            event_id    varchar(255) NOT NULL,
            event_ts    timestamp with time zone NOT NULL,
            hostname    varchar(255) NOT NULL,
            name        varchar(255) NOT NULL,
            payload     TEXT
        );"""))
        print("{} upgrade: creating index".format(__file__))
        conn.execute(text("""
                          CREATE INDEX idx_events_event_ts
                              ON alembic_example.events (event_ts)
                          """))
        conn.execute(text("""
                          CREATE INDEX idx_events_event_id
                              ON alembic_example.events (event_id)
                          """))
        print("{} upgrade: committing".format(__file__))
        transaction.commit()
        print("{} upgrade: done!".format(__file__))
    except:
        print("{} upgrade: error, rolling back!".format(__file__))
        transaction.rollback()
        raise


def downgrade():
    pass
