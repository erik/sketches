import sqlite3
import time


db = sqlite3.connect('dev.db')

with open('./schema.sql', 'r') as fp:
    schema = fp.read()
    db.cursor().executescript(schema)


class Assignees:
    @staticmethod
    def _get_all():
        cur = db.cursor()
        cur.execute('SELECT * FROM assignees;')
        return cur.fetchall()

    @classmethod
    def get(cls, name=None):
        if name is None:
            return cls._get_all()

        cur = db.cursor()
        cur.execute('SELECT * FROM assignees WHERE assignee_name = ?', name)
        return cur.fetch()


class TaskDefinitions:
    @staticmethod
    def _get_all():
        cur = db.cursor()
        cur.execute('SELECT * FROM task_definitions')
        return cur.fetchall()

    @staticmethod
    def get_overdue():

        cur = db.cursor()
        cur.execute('SELECT task_id, MAX(ts_complete) '
                    'FROM task_history '
                    "WHERE ts_complete > 0 "
                    'GROUP BY 1 '
                    'ORDER BY ts_complete DESC')

        last_done = {
            row[0]: row[1]
            for row in cur.fetchall()
        }

        now = time.time()

        return [
            dict(zip(['id', 'task_name', 'effort', 'period'], row))
            for row in TaskDefinitions._get_all()
            if last_done.get(row[0], 0) + (86400 * row[3]) < now
        ]


class TaskHistory:
    @staticmethod
    def _get_all():
        cur = db.cursor()
        cur.execute('SELECT * FROM task_history')
        return cur.fetchall()

    @staticmethod
    def get_open_tasks():
        cur = db.cursor()
        cur.execute('SELECT * FROM task_history '
                    'JOIN task_definitions USING (task_id) '
                    'JOIN assignees USING (assignee_id) '
                    'WHERE ts_complete = 0 '
                    'ORDER BY ts_assigned DESC')
        return cur.fetchall()

    @staticmethod
    def get_recent_tasks():
        cur = db.cursor()
        cur.execute('SELECT * FROM task_history '
                    'JOIN task_definitions USING (task_id) '
                    'JOIN assignees USING (assignee_id) '
                    "WHERE ts_assigned > date('now', '- 1 month') "
                    'ORDER BY ts_assigned DESC')
        return cur.fetchall()


def schedule_new_tasks():
    recent = TaskHistory.get_recent_tasks()
    overdue = TaskDefinitions.get_overdue()

    print('recent: {}, overdue: {}'.format(recent, overdue))


schedule_new_tasks()
