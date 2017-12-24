CREATE TABLE IF NOT EXISTS assignees (
       assignee_id      integer PRIMARY KEY,
       assignee_name    text
);

CREATE TABLE IF NOT EXISTS task_definitions (
       task_id        integer PRIMARY KEY,
       task_name      text,
       effort    integer,
       period    integer
);

-- TODO: Figure out if it's better to split current assignments into separate
-- TODO: table.
CREATE TABLE IF NOT EXISTS task_history (
       history_id  integer PRIMARY KEY,
       task_id     integer,
       assignee_id integer,
       ts_complete integer,
       ts_assigned integer,

       FOREIGN KEY(task_id) REFERENCES task_definitions(task_id),
       FOREIGN KEY(assignee_id) REFERENCES assignees(assignee_id)
);

INSERT INTO assignees (assignee_name) VALUES ('foo'), ('bar'), ('baz');
INSERT INTO task_definitions (task_name, effort, period) VALUES
       ('mop floor', 45, 30),
       ('clean fridge', 15, 30),
       ('clean bathroom', 30, 7),
       ('vacuum', 20, 7),
       ('countertops', 5, 3);
INSERT INTO task_history (task_id, assignee_id, ts_complete) VALUES
(1, 1, 1514092440);
