;; (progn (setf sql-postgres-program (concat default-directory "pg.sh")) (sql-postgres))

;; C-e C-xC-e

CREATE TABLE IF NOT EXISTS etest (
       id         BIGSERIAL NOT NULL,
       first_name VARCHAR(255) NOT NULL,
       last_name  VARCHAR(255) NOT NULL,
       email      VARCHAR(255)
);

-- C-<Space> C-n
-- C-<Space> M-}
-- C-cC-r

SELECT * FROM etest;


INSERT INTO etest
       (first_name, last_name, email)
VALUES
       ('Isaac', 'Schaaf', 'ischaaf@someting.com'),
       ('Kyle',  'Burton', 'kburton@someting.com');


SELECT * FROM etest;


SELECT now();
