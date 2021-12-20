package main

import (
	"database/sql"
	"fmt"
	"os"
	_ "github.com/mattn/go-sqlite3"
)

func main() {
	dbFile := "./test.db"
	fmt.Printf("in main\n")
	os.Remove(dbFile)
	db, err := sql.Open("sqlite3", dbFile)
	if err != nil {
		panic(err)
	}

	defer db.Close()
	sqls := []string{
		"create table foo (id integer not null primary key, name text)",
		"delete from foo",
	}

	for _, sql := range sqls {
		_, err = db.Exec(sql)
		if err != nil {
			panic(err)
		}
	}

	tx, err := db.Begin()
	if err != nil {
		panic(err)
	}

	stmt, err := tx.Prepare("insert into foo(id,name) values (?,?)")
	if err != nil {
		panic(err)
	}

	defer stmt.Close()
	for ii := 0; ii < 100; ii++ {
		_, err = stmt.Exec(ii, fmt.Sprintf("こんにちわ世界%03d", ii))

		if err != nil {
			panic(err)
		}
	}

	tx.Commit()

	rows, err := db.Query("select id,name from foo")
	if err != nil {
		panic(err)
	}

	defer rows.Close()

	for rows.Next() {
		var id int
		var name string
		rows.Scan(&id, &name)
		fmt.Println(id, name)
	}
	rows.Close()

	stmt, err = db.Prepare("select name from foo where id = ?")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer stmt.Close()
	var name string
	err = stmt.QueryRow("3").Scan(&name)
	if err != nil {
		fmt.Println(err)
		return
	}
	fmt.Println(name)

	_, err = db.Exec("delete from foo")
	if err != nil {
		fmt.Println(err)
		return
	}

	_, err = db.Exec("insert into foo(id, name) values(1, 'foo'), (2, 'bar'), (3, 'baz')")
	if err != nil {
		fmt.Println(err)
		return
	}

	rows, err = db.Query("select id, name from foo")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer rows.Close()
	for rows.Next() {
		var id int
		var name string
		rows.Scan(&id, &name)
		fmt.Println(id, name)
	}
	rows.Close()

}
